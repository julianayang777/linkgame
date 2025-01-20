package linkgame

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import linkgame.Board._

object Main extends IOApp {
  val rows   = 3
  val cols   = 4
  val values = 12

  sealed trait InputError
  final case object InvalidFormatError extends InputError
  final case object OutOfBoundsError   extends InputError
  final case object InvalidMatchError  extends InputError

  def gameLoop(b: Board): IO[Unit] = {
    def validateInput(board: Board, input: String): Either[InputError, (Int, Int, Int, Int)] = {
      val inputPattern = """^(\d+)\s+(\d+)\s*,\s*(\d+)\s+(\d+)$""".r
      input match {
        case inputPattern(x1, y1, x2, y2) => {
          (x1.toIntOption, y1.toIntOption, x2.toIntOption, y2.toIntOption) match {
            case (Some(x1), Some(y1), Some(x2), Some(y2)) =>
              // Check Bounds
              if (
                !(x1 > 0 && x1 < (board.length - 1) &&
                  x2 > 0 && x2 < (board.length - 1) &&
                  y1 > 0 && y1 < (board(0).length - 1) &&
                  y2 > 0 && y2 < (board(0).length - 1))
              ) {
                Left(OutOfBoundsError)
              } else
              // Check Path
              if (!isValidPath(board, (x1, y1), (x2, y2))) {
                Left(InvalidMatchError)
              } else { Right((x1, y1, x2, y2)) }
            case _                                        => Left(InvalidFormatError)
          }
        }
        case _                            => Left(InvalidFormatError)
      }
    }

    def getInputAndUpdateBoard(board: Board): IO[Board] = {
      for {
        _            <- IO.println("Choose the tiles (<x1> <y1>,<x2> <y2>): ")
        input        <- IO.readLine
        updatedBoard <- validateInput(board, input) match {
          case Left(InvalidFormatError) =>
            IO.println("Invalid Input! Please use the format \"<x1> <y1>, <x2> <y2>\"") *> (getInputAndUpdateBoard(
              board
            ))
          case Left(OutOfBoundsError)   =>
            IO.println("Invalid Coordinates! Please enter values within the board's limits") *> (getInputAndUpdateBoard(
              board
            ))
          case Left(InvalidMatchError)  =>
            IO.println(
              "Invalid Match! The tiles you selected cannot be linked. Please check the path and try again."
            ) *> (getInputAndUpdateBoard(board))
          case Right((x1, y1, x2, y2))  =>
            val updatedBoard1 = deleteTileFromBoard(board, (x1, y1))
            val updatedBoard2 = deleteTileFromBoard(updatedBoard1, (x2, y2))
            IO.pure { updatedBoard2 }
        }
      } yield updatedBoard
    }

    def loop(board: Board): IO[Unit] = {
      for {
        updatedBoard <- getInputAndUpdateBoard(board)
        _            <-
          if (updatedBoard.forall(_.forall(_ == 0))) {
            print("Congratulations!!")
            IO.unit
          } else {
            printBoard(updatedBoard) *> loop(updatedBoard)
          }
      } yield ()
    }

    for {
      _ <- IO.println("##############################")
      _ <- IO.println("           Link Game          ")
      _ <- IO.println("##############################")
      _ <- IO.println("Goal: Clear the board by matching pairs of tiles")
      _ <- IO.println("Matching rules: ")
      _ <- IO.println(" 1. Tiles can be linked if a clear path exists between them.")
      _ <- IO.println(" 2. The path can bend at most twice and cannot cross other tiles.")
      _ <- IO.println(
        "How to play: Enter coordinates of two tiles to match in the following format: <x1> <y1>,<x2> <y2>"
      )
      _ <- IO.println("Example: To match tiles at (1, 2) and (3, 4), input: 1 2, 3 4")
      _ <- IO.println("Good luck!")
      _ <- printBoard(b)
      _ <- loop(b)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      board <- initBoard(rows, cols, values)
      _     <- gameLoop(board)
    } yield ExitCode.Success
  }
}
