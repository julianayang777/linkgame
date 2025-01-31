package server

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{IO, Ref}
import cats.syntax.all._
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import io.circe.jawn
import io.circe.syntax.EncoderOps
import linkgame.game.Board.printBoard
import linkgame.game.Command.StartGame
import linkgame.game.GameState.{AwaitingPlayers, InProgress}
import linkgame.game.{Command, GameLevel, GameState}
import linkgame.player.Player
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

import java.util.UUID
import scala.concurrent.duration.DurationInt

object Route {
  def apply(wsb: WebSocketBuilder2[IO], gameRooms: Ref[IO, Map[UUID, AtomicCell[IO, GameRoom]]]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      /** curl -XPOST "localhost:8080/game/create/<level>
        * <level> := "easy" | "medium" | "hard"
        * Response:
        * - roomId - used to identify the room in the future plays
        */
      case POST -> Root / "game" / "create" / GameLevel(level) =>
        for {
          roomId   <- IO.randomUUID
          topic    <- Topic[IO, WebSocketFrame.Text]
          roomRef  <- AtomicCell
            .apply[IO]
            .of[GameRoom](GameRoom(state = AwaitingPlayers(Set.empty, level), topic = topic))
          _        <- gameRooms.update(_.updated(roomId, roomRef))
          response <- Created(roomId.toString)
        } yield response

      /** websocat -c "ws://localhost:8080/game/join/<roomId>/<playerID>"
        * <roomId>   UUID, should be valid and a room associated with it
        * <playerID>    playerId
        * <match>       -> {"type": "Match", "p1": <coordinate>, "p2": <coordinate>}
        * <coordinate> -> {"row": "<number>", "column": "<number>" }
        * Response if valid:
        * - GameStatus
        */
      case GET -> Root / "game" / "join" / roomId / playerId   =>
        for {
          roomId       <- IO(UUID.fromString(roomId))
          maybeRoomRef <- gameRooms.get.map(_.get(roomId))
          // FIXME: always creating a new player
          player        = Player(playerId)
          response     <- maybeRoomRef.fold(NotFound(s"Room $roomId is not found")) { roomRef =>
            for {
              result   <- roomRef.evalModify(_.handleCommand(Command.Join(player)))
              response <- result.fold(
                error => BadRequest(s"Failed to join room $roomId: $error"),
                newState =>
                  for {
                    topic          <- roomRef.get.map(_.topic)
                    queue          <- Queue.unbounded[IO, WebSocketFrame]
                    newStateMessage = WebSocketFrame.Text(newState.asJson.noSpaces)
                    _              <- queue.offer(newStateMessage)
                    _              <- topic.publish1(newStateMessage)
                    _              <- WebSocketHelper.scheduleStartGameIfNeeded(newState, roomRef, topic)
                    response       <- wsb.build(
                      send = WebSocketHelper.send(topic, queue),
                      receive = WebSocketHelper.receive(player, topic, queue, roomRef),
                    )
                  } yield response,
              )
            } yield response
          }
        } yield response

      /** curl "localhost:8080/game/<roomID>/status
        * <roomId>   UUID, should be valid and a room associated with it
        * Response:
        * - GameStatus
        */
      case GET -> Root / "game" / roomId / "status"            =>
        for {
          roomId       <- IO(UUID.fromString(roomId))
          maybeRoomRef <- gameRooms.get.map(_.get(roomId))
          response     <- maybeRoomRef.fold(NotFound(s"Room $roomId is not found")) { roomRef =>
            roomRef.get.flatMap(room => Ok(room.state.asJson))
          }
        } yield response
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
    ): Pipe[IO, WebSocketFrame, Unit] =
      _.foreach {
        case text: WebSocketFrame.Text =>
          jawn
            .decode[Request](text.str)
            .fold(
              e => queue.offer(WebSocketFrame.Text(s"Cannot parse ${text.str} - $e")),
              request => handleRequest(player, request, topic, queue, roomRef),
            )
        case _                         => IO.unit
      }

    private def handleRequest(
      player: Player,
      request: Request,
      topic: Topic[IO, WebSocketFrame.Text],
      queue: Queue[IO, WebSocketFrame],
      roomRef: AtomicCell[IO, GameRoom],
    ): IO[Unit] = {
      val command = request match {
        case Request.Match(p1, p2) => Command.Match(player, p1, p2)
      }
      for {
        newStateOrError <- roomRef.evalModify(_.handleCommand(command))
        _               <- newStateOrError.fold(
          error => queue.offer(WebSocketFrame.Text(s"Failed to handle $request - $error")),
          newState => {
            for {
              // _ <- scheduleStartGameIfNeeded(newState, roomRef, topic)
              _ <- topic.publish1(WebSocketFrame.Text(newState.asJson.noSpaces))
              _ <- newState
                .asInstanceOf[InProgress]
                .playerBoards
                .get(player)
                .fold(IO.println("Unexpected Error!"))(printBoard)
            } yield ()
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
            _               <- newStateOrError.traverse_ { newState =>
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
