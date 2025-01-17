package linkgame

import cats.effect.std.Random
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import cats.implicits._

object Main extends IOApp {
  val rows = 3
  val cols = 4
  val values = 8
  type Board = Vector[Vector[Int]]


  def deleteTileFromBoard(board: Board, p: (Int, Int)): Board = {
    board.updated(p._1, board(p._1).updated(p._2, 0))
  }

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
        isEmpty(d) && matchStraightLine(a, d) && matchStraightLine(c, b)

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
        board(0).indices.exists { i =>
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
    val cols = board.head.length
    for {
      _ <- IO.println("Board: \n")
      _ <- IO.println((1 until rows).foldLeft("    ")((acc, n) => acc + n.toString.padTo(3, ' ')))
    } yield (
      board.zipWithIndex.foreach {
        case (_, i) if (i == 0 || i == (cols - 2)) => println("")
        case (row, i) =>
          val output = row.tail.init.map {
            case 0 => "   "
            case n => GameLevel.toEmoji(n).padTo(3, ' ')
          }.mkString
          println(s"${i.toString.padTo(3, ' ')} $output")
      }
      )
  }

  def initBoard(rows: Int, cols: Int): IO[Board] = {
    def addBorder(board: Board): Board = {
      val emptyLine: Vector[Int] = Vector.fill(board.head.size + 2)(0)
      Vector(emptyLine) ++ board.map( row => 0 +: row :+ 0) ++ Vector(emptyLine)
    }

    // FIXME: the generated board could not have a play
    val allTiles: List[Int] = LazyList
      .continually((1 to values).flatMap(value => List(value, value)))
      .flatten
      .take(rows * cols)
      .toList

    println(s"DEBUG: All Elements: ${allTiles}")

    for {
      random <- Random.scalaUtilRandom[IO]
      shuffledTiles <- random.shuffleList(allTiles)
      innerBoard = (shuffledTiles
        .grouped(cols)
        .map(_.toVector)
        .toVector)
    } yield (addBorder(innerBoard))
  }

  def gameLoop(b: Board): IO[Unit] = {
    def readInput(board: Board): IO[Board] = {
      val inputPattern = """^(\d+)\s+(\d+)\s*,\s*(\d+)\s+(\d+)$""".r
      for {
        _ <- IO.println("Choose the tiles: ")
        input <- IO.readLine
        updatedBoard <- input match {
          case inputPattern(x1, y1, x2, y2) =>
            (x1.toIntOption, y1.toIntOption, x2.toIntOption, y2.toIntOption) match {
              case (Some(x1), Some(y1), Some(x2), Some(y2)) if
                x1 > 0 && x1 < (board.length - 1) &&
                  x2 > 0 && x2 < (board.length - 1) &&
                  y1 > 0 && y1 < (board(0).length - 1) &&
                  y2 > 0 && y2 < (board(0).length - 1) &&
                  isValidPath(board, (x1, y1), (x2, y2))
              =>
                val updatedBoard1 = deleteTileFromBoard(board, (x1, y1))
                val updatedBoard2 = deleteTileFromBoard(updatedBoard1, (x2, y2))
                IO.pure {
                  updatedBoard2
                }
              case _ =>
                println("not a valid number || out of the board || not a valid match")
                readInput(board)
            }
          case _ =>
            println("Invalid position! Please use the following format \"<x1> <y1>, <x2> <y2>\"")
            readInput(board)
        }

      } yield updatedBoard
    }

    def loop(board: Board): IO[Unit] = {
      for {
        updatedBoard <- readInput(board)
        _ <-
          if (updatedBoard.forall(_.forall(_ == 0))) {
            print("Congratulations!!")
            IO.unit
          } else {
            printBoard(updatedBoard).flatMap(_ => loop(updatedBoard))
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
      _ <- printBoard(b)
      _ <- loop(b)
    } yield ()
  }


  override def run(args: List[String]): IO[ExitCode] = {
    for {
       board <- initBoard(rows, cols)
      _ <- gameLoop(board)
    } yield ExitCode.Success
  }
}