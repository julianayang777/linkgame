package server

import linkgame.Board.Board

import java.util.UUID

case class GameResponse(id: UUID, board: Board)
