package linkgame.game

import cats.effect.IO
import cats.effect.std.Random

object Board {

  type Board = Vector[Vector[Int]]

  /** Randomly initializes the game board based on the selected level.
    * The generated board have a border of zeros's around
    */
  def initBoard(level: GameLevel): IO[Board] = {
    val (rows, cols, values) = level match {
      case GameLevel.Easy   => (6, 5, 4)
      case GameLevel.Medium => (8, 8, 10)
      case GameLevel.Hard   => (10, 10, 12)
    }

    def addBorder(board: Board): Board = {
      val emptyLine: Vector[Int] = Vector.fill(board.head.size + 2)(0)
      Vector(emptyLine) ++ board.map(row => 0 +: row :+ 0) ++ Vector(emptyLine)
    }

    val allTiles: List[Int] = LazyList
      .continually((1 to values).flatMap(value => List(value, value)))
      .flatten
      .take(rows * cols)
      .toList

    println(s"DEBUG: All Elements: ${allTiles}")

    for {
      random        <- Random.scalaUtilRandom[IO]
      shuffledTiles <- random.shuffleList(allTiles)
      innerBoard     = (shuffledTiles
        .grouped(cols)
        .map(_.toVector)
        .toVector)
      board         <-
        if (isSolvable(innerBoard)) { IO.pure { addBorder(innerBoard) } }
        else refreshBoard(innerBoard)
    } yield board

  }

  /** Prints the game board to the console, converting each value to a emoji.
    */
  def printBoard(board: Board): IO[Unit] = {
    def toEmoji(n: Int): String = {
      n match {
        case 1  => "\uD83D\uDD35" // 🔵
        case 2  => "\uD83D\uDFE1" // 🟡
        case 3  => "\uD83D\uDD34" // 🔴
        case 4  => "\uD83D\uDFE2" // 🟢
        case 5  => "\uD83D\uDFE3" // 🟣
        case 6  => "\uD83D\uDC3C" // 🐼
        case 7  => "\uD83D\uDC28" // 🐨
        case 8  => "\uD83D\uDC23" // 🐣
        case 9  => "\uD83E\uDD86" // 🦆
        case 10 => "\uD83D\uDC38" // 🐸
        case 11 => "\uD83E\uDD93" // 🦓
        case 12 => "\uD83E\uDD88" // 🦈
      }
    }

    val rows = board.length
    val cols = board.head.length
    for {
      _ <- IO.println((1 until cols - 1).foldLeft(" ".padTo(5, ' '))((acc, n) => acc + n.toString.padTo(3, ' ')))
    } yield (
      board.zipWithIndex.foreach {
        case (_, i) if (i == 0 || i == (rows - 1)) => println("")
        case (row, i)                              =>
          val output = row.tail.init.map {
            case 0 => " ".padTo(3, ' ')
            case n => toEmoji(n).padTo(3, ' ')
          }.mkString
          println(s"${i.toString.padTo(3, ' ')} $output\n")
      }
    )
  }

  /** Check if the given coordinate is within the board's valid range.
    * Returns `false` if the coordinates is either:
    * - Out of bounds (outside the board dimensions)
    * - On one of the border cells
    */
  def isCoordinateOnBoard(board: Board, coordinate: Coordinate) = {
    val rows = board.size - 1
    val cols = board.headOption.map(_.size - 1).getOrElse(0)
    coordinate.row > 0 && coordinate.row < rows && coordinate.column > 0 && coordinate.column < cols
  }

  /** Removes a specific tile located at point `p` from the board.
    */
  def deleteTileFromBoard(board: Board, p: Coordinate): Board = {
    board.updated(p.row, board(p.row).updated(p.column, 0))
  }

  /** Checks whether the board if empty or not
    */
  def isEmpty(board: Board): Boolean = board.forall(_.forall(_ == 0))

  /** Checks if there is a valid path between two points on the board.
    * A valid path can be a straight line or a line with one or two turns.
    */
  def isValidPath(board: Board, p1: (Int, Int), p2: (Int, Int)): Boolean = {
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
      isEmpty(d) && matchStraightLine(a, d) && matchStraightLine(d, b)
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

  /** Checks if there is at least one valid match.
    */
  def isSolvable(board: Board): Boolean = {
    val matchingPairs = for {
      x1    <- board.indices
      y1    <- board(x1).indices
      value1 = board(x1)(y1) if value1 != 0
      x2    <- board.indices
      y2    <- board(x2).indices if board(x2)(y2) == value1
    } yield {
      ((x1, y1), (x2, y2))
    }

    matchingPairs.exists { case (p1, p2) => isValidPath(board, p1, p2) }
  }

  /** Refresh the board until it is solvable.
    */
  def refreshBoard(board: Board): IO[Board] = {
    def loop: IO[Board] = {
      val nonEmpty: Vector[(Int, Int)] =
        for {
          (row, i)     <- board.zipWithIndex
          (element, j) <- row.zipWithIndex
          if element != 0
        } yield (i, j)

      for {
        random           <- Random.scalaUtilRandom[IO]
        shuffledNonEmpty <- random.shuffleVector(nonEmpty)
      } yield nonEmpty
        .zip(shuffledNonEmpty)
        .foldLeft(board) { case (acc, ((initialI, initialJ), (targetI, targetJ))) =>
          val element = board(initialI)(initialJ)
          acc.updated(targetI, acc(targetI).updated(targetJ, element))
        }
    }

    for {
      newBoard       <- loop
      refreshedBoard <-
        if (isSolvable(newBoard)) { IO.pure { newBoard } }
        else { refreshBoard(board) }
    } yield refreshedBoard

  }

}
