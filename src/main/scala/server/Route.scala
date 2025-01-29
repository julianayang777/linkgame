package server

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.{IO, Ref}
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.jawn
import io.circe.syntax.EncoderOps
import linkgame.game.Board.printBoard
import linkgame.game.GameState.{AwaitingPlayers, InProgress}
import linkgame.game.{Command, GameLevel}
import linkgame.player.Player
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

import java.util.UUID

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
          response     <- maybeRoomRef match {
            case Some(roomRef) =>
              for {
                result   <- roomRef.evalModify(_.handleCommand(Command.Join(Player(playerId))))
                response <- result match {
                  case Left(error)     => BadRequest(s"Failed to join room $roomId: $error")
                  case Right(newState) =>
                    val newStateMessage = WebSocketFrame.Text(newState.asJson.noSpaces)

                    for {
                      topic    <- roomRef.get.map(_.topic)
                      queue    <- Queue.unbounded[IO, WebSocketFrame.Text]
                      _        <- queue.offer(newStateMessage)
                      _        <- topic.publish1(newStateMessage)
                      send      = Stream.fromQueueUnterminated(queue) merge topic.subscribeUnbounded
                      response <- wsb.build(
                        send = send,
                        receive = _.foreach {
                          case text: WebSocketFrame.Text =>
                            jawn.decode[Request](text.str) match {
                              case Left(e)        => queue.offer(WebSocketFrame.Text(s"Cannot parse ${text.str} - $e"))
                              case Right(request) =>
                                val command = request match {
                                  case Request.Match(p1, p2) => Command.Match(p1, p2)
                                }
                                for {
                                  newStateOrError <- roomRef.evalModify(_.handleCommand(command))
                                  _               <- newStateOrError.fold(
                                    error => queue.offer(WebSocketFrame.Text(s"Failed to handle $request - $error")),
                                    newState =>
                                      topic.publish1(WebSocketFrame.Text(newState.asJson.noSpaces))
                                      // DEBUG
                                        *> printBoard(newState.asInstanceOf[InProgress].board),
                                  )
                                } yield ()
                            }
                          case _                         => IO.unit
                        },
                      )
                    } yield response
                }
              } yield response
            case None          => NotFound(s"Room $roomId is not found")
          }
        } yield response
    }

}
