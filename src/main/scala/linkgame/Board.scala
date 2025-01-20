package linkgame

import cats.effect.IO
import cats.effect.std.Random


object Board {
  type Board = Vector[Vector[Int]]

  def initBoard(rows: Int, cols: Int, values: Int): IO[Board] = {
    def addBorder(board: Board): Board = {
      val emptyLine: Vector[Int] = Vector.fill(board.head.size + 2)(0)
      Vector(emptyLine) ++ board.map(row => 0 +: row :+ 0) ++ Vector(emptyLine)
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

  def printBoard(board: Board): IO[Unit] = {
    val rows = board.length
    val cols = board.head.length
    for {
      _ <- IO.println("Board: \n")
      _ <- IO.println((1 until cols - 1).foldLeft(" ".padTo(5, ' '))((acc, n) => acc + n.toString.padTo(3, ' ')))
    } yield (
      board.zipWithIndex.foreach {
        case (_, i) if (i == 0 || i == (rows - 1)) => println("")
        case (row, i) =>
          val output = row.tail.init.map {
            case 0 => " ".padTo(3, ' ')
            case n => GameLevel.toEmoji(n).padTo(3, ' ')
          }.mkString
          println(s"${i.toString.padTo(3, ' ')} $output\n")
      }
      )
  }

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

}
