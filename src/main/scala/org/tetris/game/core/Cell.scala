package org.tetris.game.core

case class Cell(x: Int, y: Int) {
  def isMoveAllowed(move: Move, board: Board): Boolean = {
    val nearestCells = board.cellsByCoor(x + move.spanX, y + move.spanY)
    val moved = Cell(x + move.spanX, y + move.spanY)
    moved.isInBorders(board) && nearestCells.isEmpty
  }

  def isInBorders(board: Board): Boolean =
    x >= 0 && x <= board.width- 1 && y >= 0 /*&& y <= board.height- 1*/

  def makeMove(move: Move, board: Board): Cell =
    if (isMoveAllowed(move, board)) Cell(x + move.spanX, y + move.spanY)
    else this
}
