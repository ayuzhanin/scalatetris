package org.tetris.game.core

case class Board(cells: List[Cell], height: Int, width: Int) {
  def cellsByCoor(x: Int, y: Int): List[Cell] = cells filter { cell => cell.x == x && cell.y == y }

  def neighbours(cell: Cell): List[Cell] = cells filter {
    c =>
      ((c.x + 1 == cell.x || c.x - 1 == cell.x) && c.y == cell.y) ||
        (c.y + 1 == cell.y || c.y - 1 == cell.y) && c.x == cell.x
  }

  private def cellsWithIndexY(index: Int): List[Cell] = cells.filter(_.y == index)

  def highest: Cell = cells match {
    case head::tail => cells maxBy( _.y )
    case Nil => Cell(0, 0)
  }

  def clear = {
    val clearCandidates = (0 until height).map(cellsWithIndexY).toList
    val filtered = clearCandidates.filterNot(_.size == width).flatten
    Board(
      filtered,
      height,
      width
    )
  }

  def consume(block: Block) = Board(block.cells ++ cells, height, width)

  private def isLineEmpty(lineIndex: Int): Boolean = !(cells exists { _.y == lineIndex })

  private def shiftDown(lineIndex: Int): Board =
    if (isLineEmpty(lineIndex - 1) && !isLineEmpty(lineIndex))
      Board(
        cells map { cell =>
          if (cell.y == lineIndex) cell.makeMove(Down(), this)
          else cell
        }, height, width)
    else this

  def pushDown: Board = {
    def shiftDownAll(board: Board): Board =
      (1 until height).foldLeft(board)((board, index) => board shiftDown index)

    def isShiftAllowed(board: Board): Boolean =
      (1 until height) exists { index => board.isLineEmpty(index - 1) && !board.isLineEmpty(index) }

    def until(condition: Board => Boolean, action: Board => Board)(board: Board): Board =
      if (!condition(board)) board
      else until(condition, action)(action(board))

    until(isShiftAllowed, shiftDownAll)(this)
  }
}
