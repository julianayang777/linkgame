package server

import cats.effect.std.{AtomicCell, Queue}
import cats.effect.IO
import fs2.concurrent.Topic
import linkgame.{GameLevel, GameSession, Player}
import org.http4s.HttpRoutes
import io.circe.generic.auto._
import linkgame.Board.{Board, deleteTileFromBoard, initBoard, isEmpty, isSolvable, isValidPath, printBoard, refreshBoard}
import linkgame.GameStatus.{Finished, InProgress}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import fs2.{Pipe, Stream}
import org.http4s.websocket.WebSocketFrame
import org.http4s.server.websocket.WebSocketBuilder2

import java.util.UUID
import scala.util.Try

object Route {
  sealed trait InputError

  final case object InvalidSessionError  extends InputError
  final case object OutOfBoundsError  extends InputError
  final case object InvalidMatchError extends InputError

  def validateCommand(board: Board, p1: (Int, Int), p2: (Int, Int)): Either[InputError, Unit] = {
    val (x1, y1) = p1
    val (x2, y2) = p2

    if (
      !(x1 > 0 && x1 < (board.length - 1) &&
        x2 > 0 && x2 < (board.length - 1) &&
        y1 > 0 && y1 < (board(0).length - 1) &&
        y2 > 0 && y2 < (board(0).length - 1))
    ) { Left(OutOfBoundsError) }
    else if (!isValidPath(board, (x1, y1), (x2, y2)))
    { Left(InvalidMatchError) }
    else { Right(()) }
  }

  def apply(wsb: WebSocketBuilder2[IO],
            ref: AtomicCell[IO, Map[UUID, GameSession]],
            q: Queue[IO, WebSocketFrame],
            t: Topic[IO, WebSocketFrame]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    /** curl -XPOST "localhost:8080/game/start/<level>
      * <level> := "easy" | "medium" | "hard"
      * Response:
      * - SessionID - used to identify the session in the future plays
      * - known as GameState, so that client knows what to play
      */
    case POST -> Root / "game" / "start" / GameLevel(level) =>
      for {
        // TODO player
        sessionId   <- IO.randomUUID
        board       <- initBoard(level)
        player = Player("player1")
        gameSession <- IO.pure { GameSession(board, InProgress, player) }
        _           <- ref.update(_.updated(sessionId, gameSession))
        // DEBUG
        _           <- printBoard(board)
        response    <- Ok(GameResponse(sessionId, board))
      } yield response

    /** curl -XPOST "localhost:8080/game/play/<id>" -d '{"x1": <x1>, "y1": <y1>, "x2": <x2>, "y2": <y2>}' -H "Content-Type: application/json
      * <id> UUID, should be valid and a session associated with it
      * <x1>, <y1>, <x2>, <y2> Integer, should be in the board, the match must be valid
      * Response if valid:
      * - GameSession - with the updated board and status of the game, so that client knows what to play next
      */
    case req @ POST -> Root / "game" / "play" / sessionId   =>
      Try(UUID.fromString(sessionId)).toOption match {
        case None     => BadRequest("Invalid ID!")
        case Some(id) => {
          for {
            command  <- req.as[Command]
            (p1, p2)  = ((command.x1, command.y1), (command.x2, command.y2))
            session  <- ref.evalModify { map =>
              map.get(id) match {
                case Some(session) =>
                  validateCommand(session.board, p1, p2) match {
                    case Left(error) => IO.pure(map, Left(error))
                    case Right(()) => {
                      val updatedBoard1 = deleteTileFromBoard(session.board, p1)
                      val updatedBoard2 = deleteTileFromBoard(updatedBoard1, p2)
                      for {
                        newBoard      <-
                          if (isSolvable(updatedBoard2)) { IO.pure { updatedBoard2 } }
                          else { refreshBoard(updatedBoard2) }
                        newStatus      = if (isEmpty(updatedBoard2)) Finished else InProgress
                        updatedSession = session.copy(newBoard, newStatus)
                      } yield (map.updated(id, updatedSession), Right(updatedSession))
                    }
                  }
                case None          => IO.pure((map, Left(InvalidSessionError)))
              }
            }
            response <- session match {
              case Left(InvalidSessionError) => BadRequest("Session ID not found")
              case Left(OutOfBoundsError) => BadRequest("Point is out of bounds")
              case Left(InvalidMatchError) => BadRequest("Invalid Match")
              case Right(session) => printBoard(session.board) *> Ok(session)
            }
          } yield response
        }
      }


//    case GET -> Root / "ws" / sessionId =>
    //      def send(id: UUID): Stream[IO, WebSocketFrame] = t.subscribe(maxQueued = 1000)

    //      def receive(id: UUID): Pipe[IO, WebSocketFrame, Unit] = in => in.foreach(q.offer)

    //      Try(UUID.fromString(sessionId)).toOption match {
    //        case None => BadRequest("Invalid ID!")
    //        case Some(id) => {
    //          wsb.build(send(id), receive(id))
    //        }
    //      }

    case GET -> Root / "ws"  =>
      val send: Stream[IO, WebSocketFrame] = t.subscribe(maxQueued = 1000)

      val receive: Pipe[IO, WebSocketFrame, Unit] = in => in.foreach(q.offer)

      wsb.build(send, receive)
  }

}
