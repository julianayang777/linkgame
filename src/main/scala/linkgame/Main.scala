package linkgame

import cats.effect.{ExitCode, IO, IOApp}

import scala.util.Random

object Main extends IOApp {
  val rows = 3
  val cols = 4
  val values = 4
  type Board = Array[Array[Int]]


  def isValidPath(board: Board, p1: (Int, Int), p2: (Int, Int)): Boolean = {
    // FIXME: Not considering that lines can be outside of the board
    def isEmpty(p: (Int, Int)): Boolean = board(p._1)(p._2) == 0

    def basicCondition(a: (Int, Int) = p1, b: (Int, Int) = p2): Boolean = {
      val (x1, y1) = a
      val (x2, y2) = b

      a != b && board(x1)(y1) == board(x2)(y2) && !isEmpty(a) && !isEmpty(b)
    }

    def matchStraightLine(a: (Int, Int) = p1, b: (Int, Int) = p2): Boolean = {
      val (x1, y1) = a
      val (x2, y2) = b
      if (x1 == x2) {
        ((y1.min(y2) + 1) until y1.max(y2)).forall(y => isEmpty((x1, y)))
      } else if (y1 == y2) {
        ((x1.min(x2) + 1) until x1.max(x2)).forall(x => isEmpty((x, y1)))
      } else {
        false
      }
    }

    def matchOneTurnLine(a: (Int, Int) = p1, b: (Int, Int) = p2): Boolean = {
      // the turn must occur at point c = (x1, y2) or c = (x2, y1)
      // a -> c -> b
      val c = (a._1, b._2)
      val d = (b._1, a._2)
      isEmpty(c) && matchStraightLine(a, c) && matchStraightLine(c, b) ||
        isEmpty(d) && matchStraightLine(a, d) && matchStraightLine(c, d)

    }

    def matchTwoTurnLine(a: (Int, Int) = p1, b: (Int, Int) = p2): Boolean = {
      // the turn must occur at a point c that lies either on the
      // same row or same column as either a or b.
      // (1) Same Column => c1 = (i, y1) && c2 = (i, y2)
      // (2) Same row => c1 = (x1, i) && c2 = (x2, i)
      // (a -> c1) ++ (c1 -> c2 -> b)

      // Case 1
      (board.indices.exists { i =>
        val c1 = (i, a._2)
        val c2 = (i, b._2)
        i != a._2 && isEmpty(c1) && matchStraightLine(a, c1) && matchOneTurnLine(c1, b) ||
          i != b._2 && isEmpty(c2) && matchStraightLine(b, c2) && matchOneTurnLine(c2, a)
      }

        ||

        // Case 2
        board.indices.exists { i =>
          val c1 = (a._1, i)
          val c2 = (b._1, i)
          i != a._1 && isEmpty(c1) && matchStraightLine(a, c1) && matchOneTurnLine(c1, b) ||
            i != b._1 && isEmpty(c2) && matchStraightLine(b, c2) && matchOneTurnLine(c2, a)
        })

    }

    basicCondition() && (matchStraightLine() || matchOneTurnLine() || matchTwoTurnLine())

  }

  def printBoard(board: Board): IO[Unit] = {
    val rows = board.length
    for {
      _ <- IO.println("Board: \n")
      _ <- IO.println((0 to rows).foldLeft("  ")((acc, n) => acc + "  " + n))
      _ <- IO.println("")
    } yield (
      board.zipWithIndex.foreach {
        case (line, i) =>
          print(s"$i   ")
          println(line.foldRight("")((t, acc) =>
            (if (t == 0) {
              "   "
            } else {
              t
            })
              + "  " + acc
          ))
      }
      )
  }

  def initBoard(rows: Int, cols: Int): Board = {
    // FIXME: the generated board could not have a play
    val allTiles: List[Int] = LazyList
      .continually((1 to 8).flatMap(value => List(value, value)))
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
                  x2 >= 0 && x2 < board.length &&
                  y1 >= 0 && y1 < board(0).length &&
                  y2 >= 0 && y2 < board(0).length &&
                  isValidPath(board, (x1, y1), (x2, y2))
              =>
                board(x1)(y1) = 0
                board(x2)(y2) = 0
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
          if (updatedBoard.forall(_.forall(_ != 0))) {
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