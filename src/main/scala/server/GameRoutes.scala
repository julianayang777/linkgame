package server

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{IO, Ref}
import cats.syntax.all._
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import linkgame.game.Board.{Path, printBoard}
import linkgame.game.Command.StartGame
import linkgame.game.GameState.{AwaitingPlayers, InProgress, Win}
import linkgame.game.{Command, GameLevel, GameState}
import linkgame.player.Player
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.server.AuthMiddleware
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{AuthedRoutes, HttpRoutes}
import server.leaderboard.LeaderboardService
import server.player.PlayerService
import server.player.PlayerService.PlayerId

import java.util.UUID
import scala.concurrent.duration.DurationInt

object GameRoutes {
  def apply(
    wsb: WebSocketBuilder2[IO],
    gameRooms: Ref[IO, Map[UUID, AtomicCell[IO, GameRoom]]],
    playerService: PlayerService,
    leaderboardService: LeaderboardService,
    authMiddleware: AuthMiddleware[IO, PlayerId],
  ): HttpRoutes[IO] = {

    authMiddleware(
      AuthedRoutes.of[PlayerId, IO] {
        /** curl -XPOST "localhost:8080/game/create/<nplayers>/<level>"
          * <nplayers> - number of players to start the room
          * <level> := "easy" | "medium" | "hard"
          * Response: (Created)
          * - roomId - used to identify the room in the future plays
          * Fail:
          * - BadRequest("Invalid number of players: must be greater than 0." |
          *              "Invalid number of players: could not parse the input as an integer.")
          */
        case POST -> Root / "game" / "create" / nplayers / GameLevel(level) as _ =>
          nplayers.toIntOption match {
            case Some(requiredPlayers) if requiredPlayers > 0 =>
              for {
                roomId   <- IO.randomUUID
                topic    <- Topic[IO, WebSocketFrame.Text]
                roomName <- gameRooms.get.map(rooms => f"Room #${rooms.size + 1}%05d")
                roomRef  <- AtomicCell
                  .apply[IO]
                  .of[GameRoom](
                    GameRoom(name = roomName, state = AwaitingPlayers(level, requiredPlayers, Set.empty), topic = topic)
                  )
                _        <- gameRooms.update(_.updated(roomId, roomRef))
                response <- Created(roomId.toString)
              } yield response
            case Some(_) => BadRequest("Invalid number of players: must be greater than 0.")
            case None    => BadRequest("Invalid number of players: could not parse the input as an integer.")
          }

        /** websocat -c "ws://localhost:8080/game/join/<roomId>"
          * <roomId>   UUID, should be valid and a room associated with it
          * <match>       -> {"type": "Match", "p1": <coordinate>, "p2": <coordinate>}
          * <coordinate> -> {"row": "<number>", "column": "<number>" }
          * Response if valid:
          * - GameStatus
          * Fail:
          * - BadRequest("Failed to retrieve player <playerId>: <error>" |
          *             "Failed to join room <roomId>: <error>")
          * - WS.Text("Failed to handle <request> - <error>")
          * - NotFound("Room <roomId> not found.")
          */
        case GET -> Root / "game" / "join" / roomId as playerId                  =>
          for {
            roomId       <- IO(UUID.fromString(roomId))
            maybeRoomRef <- gameRooms.get.map(_.get(roomId))
            maybePlayer  <- playerService.getPlayer(playerId)
            response     <- maybePlayer.fold(
              error => BadRequest(s"Failed to retrieve player $playerId: $error"),
              player =>
                maybeRoomRef.fold(NotFound(s"Room $roomId not found")) { roomRef =>
                  for {
                    result   <- roomRef.evalModify(_.handleCommand(Command.Join(player)))
                    response <- result.fold(
                      error => BadRequest(s"Failed to join room $roomId: $error"),
                      { case (_, newState) =>
                        for {
                          room           <- roomRef.get
                          topic           = room.topic
                          queue          <- Queue.unbounded[IO, WebSocketFrame]
                          newStateMessage = WebSocketFrame.Text(
                            newState.asJson.mapObject(state => state.add("name", room.name.asJson)).noSpaces
                          )
                          _              <- queue.offer(newStateMessage)
                          _              <- topic.publish1(newStateMessage)
                          _              <- WebSocketHelper.scheduleStartGameIfNeeded(newState, roomRef, topic)
                          response       <- wsb.build(
                            send = WebSocketHelper.send(topic, queue),
                            receive = WebSocketHelper.receive(player, topic, queue, roomRef, leaderboardService),
                          )
                        } yield response
                      },
                    )
                  } yield response
                },
            )
          } yield response

        /** curl "localhost:8080/game/<roomID>/status"
          * <roomId>   UUID, should be valid and a room associated with it
          * Response:
          * - GameStatus
          * Fail:
          * - NotFound("Room <roomId> not found")
          */
        case GET -> Root / "game" / roomId / "status" as _                       =>
          for {
            roomId       <- IO(UUID.fromString(roomId))
            maybeRoomRef <- gameRooms.get.map(_.get(roomId))
            response     <- maybeRoomRef.fold(NotFound(s"Room $roomId not found.")) { roomRef =>
              roomRef.get.flatMap { room =>
                Ok(room.state.asJson.mapObject(state => state.add("name", room.name.asJson)))
              }
            }
            _            <- IO.println(response)
          } yield response

        /** curl "localhost:8080/game/rooms"
          * Response:
          *  - List of GameRoomResponses
          * Fail:
          * - BadRequest("Failed to retrieve player <playerId>: <error>"),
          */
        case GET -> Root / "game" / "rooms" as playerId                          =>
          for {
            maybePlayer <- playerService.getPlayer(playerId)
            response    <- maybePlayer.fold(
              error => BadRequest(s"Failed to retrieve player $playerId: $error"),
              player =>
                for {
                  roomsList         <- gameRooms.get.map(_.toList)
                  gameRoomResponses <- roomsList.traverse { case (id, roomCell) =>
                    roomCell.get.map { room => GameRoomResponse.fromGameState(id, player, room.name, room.state) }
                  }
                  response          <- Ok(gameRoomResponses.asJson)
                } yield response,
            )
          } yield response
      }
    )
  }

  private object WebSocketHelper {
    def send(topic: Topic[IO, WebSocketFrame.Text], queue: Queue[IO, WebSocketFrame]): Stream[IO, WebSocketFrame] =
      Stream
        .awakeEvery[IO](30.seconds)
        .map(_ => WebSocketFrame.Ping())
        .foreach(queue.offer) merge
        Stream.fromQueueUnterminated(queue) merge topic.subscribeUnbounded

    def receive(
      player: Player,
      topic: Topic[IO, WebSocketFrame.Text],
      queue: Queue[IO, WebSocketFrame],
      roomRef: AtomicCell[IO, GameRoom],
      leaderboardService: LeaderboardService,
    ): Pipe[IO, WebSocketFrame, Unit] =
      _.foreach {
        case text: WebSocketFrame.Text =>
          jawn
            .decode[Request](text.str)
            .fold(
              e => queue.offer(WebSocketFrame.Text(s"Cannot parse ${text.str} - $e")),
              request => handleRequest(player, request, topic, queue, roomRef, leaderboardService),
            )
        case _                         => IO.unit
      }

    private def sendLinkPath(queue: Queue[IO, WebSocketFrame], maybePath: Option[Path]): IO[Unit] = {
      maybePath match {
        case Some(path) =>
          val pathMessage = WebSocketFrame.Text(path.asJson.noSpaces)
          IO.println(s"pathMessage: $pathMessage") *> queue.offer(pathMessage)
        case None       => IO.println("hello") *> IO.unit
      }
    }

    private def handleRequest(
      player: Player,
      request: Request,
      topic: Topic[IO, WebSocketFrame.Text],
      queue: Queue[IO, WebSocketFrame],
      roomRef: AtomicCell[IO, GameRoom],
      leaderboardService: LeaderboardService,
    ): IO[Unit] = {
      val command = request match {
        case Request.Match(p1, p2) => Command.Match(player, p1, p2)
        case _                     => Command.InvalidCommand
      }
      for {
        newStateOrError <- roomRef.evalModify(_.handleCommand(command))
        _               <- newStateOrError.fold(
          error => queue.offer(WebSocketFrame.Text(s"Failed to handle $request - $error")),
          {
            case (maybePath, newState) => {
              for {
                _ <- topic.publish1(WebSocketFrame.Text(newState.asJson.noSpaces))
                _ <- sendLinkPath(queue, maybePath)
                _ <- newState match {
                  case inProgress: InProgress                    =>
                    inProgress.playerBoards
                      .get(player)
                      .fold(IO.println("Unexpected Error!"))(printBoard)
                  case Win(gameLevel, player, _, completionTime) =>
                    leaderboardService.addScore(gameLevel, player.name, completionTime.toMillis)
                  case _                                         => IO.unit
                }
              } yield ()
            }
          },
        )
      } yield ()
    }

    def scheduleStartGameIfNeeded(
      gameState: GameState,
      roomRef: AtomicCell[IO, GameRoom],
      topic: Topic[IO, WebSocketFrame.Text],
    ): IO[Unit] = {
      gameState match {
        case state: GameState.GameStartsSoon =>
          val schedule = for {
            _               <- IO.sleep(state.startIn)
            newStateOrError <- roomRef.evalModify(_.handleCommand(StartGame))
            _               <- newStateOrError.traverse_ { case (_, newState) =>
              topic.publish1(WebSocketFrame.Text(newState.asJson.noSpaces))
            }
          } yield ()
          // Start a fiber to not block the "main" server
          schedule.start.void
        case _                               => IO.unit
      }
    }

  }

}
