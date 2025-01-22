package server

import cats.effect.{IO, Ref}
import cats.implicits.catsSyntaxApplicativeId
import linkgame.{GameLevel, GameSession}
import org.http4s.HttpRoutes
import io.circe.generic.auto._
import linkgame.Board.{deleteTileFromBoard, initBoard, isEmpty, isSolvable, isValidPath, printBoard, refreshBoard}
import linkgame.GameStatus.{Finished, InProgress}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._

import java.util.UUID
import scala.util.Try

object Route {
  def apply(ref: Ref[IO, Map[UUID, GameSession]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    /** curl -XPOST "localhost:8080/game/start/<level>
      * <level> := "easy" | "medium" | "hard"
      * Response:
      * - SessionID - used to identify the session in the future plays
      * - known as GameState, so that client knows what to play
      */
    case POST -> Root / "game" / "start" / GameLevel(level) =>
      for {
        sessionId   <- IO.randomUUID
        board       <- initBoard(level)
        gameSession <- IO.pure { GameSession(board, InProgress) }
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
            session  <- ref.modify { map =>
              // TODO: shoud i do commmand validation? (the points are in the bound)
              map.get(id) match {
                case Some(session) =>
                  if (isValidPath(session.board, p1, p2)) {
                    val updatedBoard1  = deleteTileFromBoard(session.board, p1)
                    val updatedBoard2  = deleteTileFromBoard(updatedBoard1, p2)
                    val newStatus = if (isEmpty(updatedBoard2)) Finished else InProgress
                    // TODO: refresh board when there is not any possible play
                    val updatedSession = session.copy(updatedBoard2, newStatus)
                    (map.updated(id, updatedSession), Some(updatedSession))
                  } else {
                    // TODO: Distinguish errors
                    (map, None)
                  }
                case None          => (map, None)
              }
            }
            response <- session match {
              case Some(session) =>
                // DEBUG
                printBoard(session.board) *>
                  Ok(session)
              case None          => BadRequest("Session ID not found or Invalid match.")
            }
          } yield response
        }
      }

  }

}
