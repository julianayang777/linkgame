package linkgame

import cats.effect.std.AtomicCell
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import linkgame.game.Board._
import linkgame.game.GameLevel.{Easy, Hard, Medium}
import linkgame.game.GameState.{GameError, InProgress}
import linkgame.game.{Coordinate, GameLevel, GameState}
import linkgame.player.Player
import linkgame.utils.Format._

object Main extends IOApp {

  sealed trait InputError
  final case object InvalidFormatError extends InputError
  final case object OutOfBoundsError   extends InputError
  final case object InvalidMatchError  extends InputError

  /** Parses the input string to determine the game level.
    */
  def parseGameLevel(input: String): IO[GameLevel] = {
    input.toIntOption match {
      case Some(1) => IO.pure(Easy)
      case Some(2) => IO.pure(Medium)
      case Some(3) => IO.pure(Hard)
      case _       =>
        IO.println(s"${Red("Invalid number!")} Please enter a valid number (${Bold("1, 2, 3")}).") *>
          IO.readLine.flatMap(parseGameLevel)
    }
  }

  def parseCoordinates(input: String): IO[(Coordinate, Coordinate)] = {
    val inputPattern = """^(\d+)\s+(\d+)\s*,\s*(\d+)\s+(\d+)$""".r
    input match {
      case inputPattern(x1, y1, x2, y2) =>
        (x1.toIntOption, y1.toIntOption, x2.toIntOption, y2.toIntOption) match {
          case (Some(x1), Some(y1), Some(x2), Some(y2)) => IO.pure { (Coordinate(x1, y1), Coordinate(x2, y2)) }
          case _                                        =>
            IO.println(
              s"${Red("Invalid Number!")} Please use the format ${Bold("<x1> <y1>, <x2> <y2>")}"
            ) *> IO.readLine.flatMap(parseCoordinates)
        }
      case _                            =>
        IO.println(
          s"${Red("Invalid Input!")} Please use the format ${Bold("<x1> <y1>, <x2> <y2>")}"
        ) *> IO.readLine.flatMap(parseCoordinates)
    }
  }

  /** Main Loop of the game
    */
  def gameLoop(player: Player, ref: AtomicCell[IO, GameState]): IO[Unit] = {
    def loop(ref: AtomicCell[IO, GameState]): IO[Unit] = {
      for {
        _               <- IO.println(s"Choose the tiles (${Bold("<x1> <y1>, <x2> <y2>")}): ")
        input           <- IO.readLine
        points          <- parseCoordinates(input)
        (p1, p2)         = points
        newStateOrError <- ref.evalModify { state =>
          state.attemptMatch(player, p1, p2) match {
            case Left(e)         =>
              IO.pure {
                (state, Left(e))
              }
            case Right(newState) =>
              newState.flatMap { case (path, state) =>
                IO.println(s"path: $path") *>
                  IO.pure {
                    (state, Right(state))
                  }
              }
          }
        }
        _               <- newStateOrError.fold(
          error =>
            error match {
              case GameError.CoordinatesOutOfBounds =>
                IO.println(
                  Red(s"${Red("Invalid Coordinates!")} Please enter values within the board's limits")
                ) *> (loop(ref))
              case GameError.InvalidMatch           =>
                IO.println(
                  Red(
                    s"${Red("Invalid Match!")} The tiles you selected cannot be linked. Please check the path and try again."
                  )
                ) *> (loop(ref))
              case _                                => IO.println(Red(s"${Red("Unexpected Error!")}"))
            },
          newState =>
            newState match {
              case InProgress(_, playerBoards, _)              =>
                playerBoards.get(player).fold(IO.println(Red(s"${Red("Unexpected Error!")}"))) { board =>
                  printBoard(board).flatMap(_ => loop(ref))
                }
              case GameState.Win(_, winner, _, completionTime) =>
                IO.println(Green(s"Congratulations ${winner.name}!!! Time taken: ${completionTime}"))
              case _                                           => IO.println(Red(s"${Red("Unexpected Error!")}"))
            },
        )
      } yield ()
    }

    for {
      stateRef <- ref.get
      _        <- IO.println(s"${Bold("Goal")}: Clear the board by matching pairs of tiles")
      _        <- IO.println(s"${Bold("Matching rules:")} ")
      _        <- IO.println(s" ${Bold("1.")} Tiles can be linked if a clear path exists between them.")
      _        <- IO.println(s" ${Bold("2.")} The path can bend at most twice and cannot cross other tiles.")
      _        <- IO.println(
        s"${Bold("How to play:")} Enter coordinates of two tiles to match in the following format: <x1> <y1>,<x2> <y2>"
      )
      _        <- IO.println(s"${Bold("Example:")} To match tiles at (1, 2) and (3, 4), input: 1 2, 3 4")
      _        <- IO.println("Good luck!\n")
      _        <- stateRef
        .asInstanceOf[InProgress]
        .playerBoards
        .get(player)
        .fold(IO.println(Red(s"${Red("Unexpected Error!")}")))(printBoard)
      _        <- loop(ref)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _            <- IO.println("##############################")
      _            <- IO.println("           Link Game          ")
      _            <- IO.println("##############################")
      _            <- IO.print(s"Enter your username: ")
      username     <- IO.readLine
      player        = Player(username)
      _            <- IO.println(
        s"Choose a game level by entering the corresponding number:\n${Bold("1")} - Easy\n${Bold("2")} - Medium\n${Bold("3")} - Hard"
      )
      input        <- IO.readLine
      level        <- parseGameLevel(input)
      board        <- initBoard(level)
      startInstant <- IO.realTimeInstant
      stateRef     <- AtomicCell.apply[IO].of[GameState](InProgress(level, Map(player -> board), startInstant))
      _            <- gameLoop(player, stateRef)

    } yield ExitCode.Success
  }
}
