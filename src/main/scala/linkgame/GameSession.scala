package linkgame

import linkgame.Board.Board

final case class GameSession(board: Board, status: GameStatus)