package linkgame

import cats.effect.{ExitCode, IO, IOApp}

import scala.util.Random

object Main extends IOApp {
  val rows = 3
  val cols = 4
  val values  = 4
  type Board = Array[Array[Tile]]
  type TileValue = Int
  case class Tile(value: TileValue){
    def isEmpty: Boolean = (value == 0)
  }


  def isValidPath(t1: Tile, t2: Tile): Boolean = {
    // TODO: not implemented
    true
  }

  def printBoard(board: Board) : IO[Unit] = {
    val rows = board.length
    for {
      _ <- IO.println("Board: \n")
      _ <- IO.println((0 to rows).foldLeft("  ")((acc, n) => acc + "  " +  n))
      _ <- IO.println("")
    } yield (
          board.zipWithIndex.foreach {
            case (line, i) =>
              print(s"$i   ")
              println(line.foldRight("")((t, acc) =>
                (if (t == null) { " null " }
                else if (t.value == 0) { "   " }
                else { t.value })
                  + "  " + acc
              ))
          }
      )
  }

  def initBoard(rows: Int, cols: Int): Board = {
    val allTiles: List[Tile] = LazyList
      .continually((1 to 8).flatMap(value => List(Tile(value), Tile(value))))
      .flatten
      .take(rows * cols)
      .toList

    println(s"All Elements: ${allTiles}")

    val r = new Random()
    val shuffledTiles = r.shuffle(allTiles)
    shuffledTiles
      .grouped(cols)
      .map(_.toArray)
      .toArray
  }

  def gameLoop(board: Board): IO[Unit] = {
    def readInput: IO[Board] = {
      val inputPattern = """^(\d+)\s+(\d+),\s*(\d+)\s+(\d+)$""".r
      for {
        _ <- IO.println("Choose the tiles: ")
        input <- IO.readLine
        _ <- input match {
          case inputPattern(x1, y1, x2, y2) =>
            (x1.toIntOption, y1.toIntOption, x2.toIntOption, y2.toIntOption) match {
              case (Some(x1), Some(y1), Some(x2), Some(y2)) if
                x1 >= 0 && x1 < board.length &&
                x2 >= 0 && x2 < board(0).length &&
                y1 >= 0 && y1 < board.length &&
                y2 >= 0 && y2 < board(0).length &&
                  isValidPath(board(x1)(y1), board(x2)(y2))
              =>
                board(x1)(y1) = board(x1)(y1).copy(0)
                board(x2)(y2) = board(x2)(y2).copy(0)
                IO.unit
              case _ =>
                println("not a valid number || out of the board || not a valid match")
                readInput
            }
          case _ =>
            println("Invalid position! Please use the following format \"<x1> <y1>, <x2> <y2>\"")
            readInput
        }

      } yield board
    }

    def loop: IO[Unit] = {
      for {
        updatedBoard <- readInput
        _ <-
          if (updatedBoard.forall(_.forall(_.isEmpty))){
            print("Congratulations!!")
            IO.unit
          } else {
            printBoard(updatedBoard).flatMap(_ => loop)
          }
      } yield ()
    }

    for {
      _ <- IO.println("##############################")
      _ <- IO.println("           Link Game          ")
      _ <- IO.println("##############################")
      _ <- IO.println("Goal: TODO_____")
      _ <- IO.println("How to play: TODO_____")
      _ <- IO.println("Example: TODO_____")
      _ <- IO.println("Input format: <x1> <y1>,<x2> <y2>")
      _ <- printBoard(board)
      _ <- loop
    } yield ()
  }


  override def run(args: List[String]): IO[ExitCode] = {
    val board = initBoard(rows, cols)
    gameLoop(board).as(ExitCode.Success)
  }
}