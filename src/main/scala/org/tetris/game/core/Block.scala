package org.tetris.game.core

sealed abstract class BlockType

object I extends BlockType

object J extends BlockType

object L extends BlockType

object O extends BlockType

object S extends BlockType

object T extends BlockType

object Z extends BlockType

object Block {
  def apply(x: Int, y: Int, blockType: BlockType): Block = blockType match {
    case I => Block(List(Cell(x - 1, y), Cell(x - 2, y), Cell(x, y), Cell(x + 1, y)), blockType)
    case J => Block(List(Cell(x, y), Cell(x - 1, y), Cell(x + 1, y), Cell(x + 1, y - 1)), blockType)
    case L => Block(List(Cell(x, y - 1), Cell(x - 1, y - 1), Cell(x + 1, y - 1), Cell(x + 1, y)), blockType) //ops
    case O => Block(List(Cell(x - 1, y - 1), Cell(x, y - 1), Cell(x - 1, y), Cell(x, y)), blockType) //ops
    case S => Block(List(Cell(x, y), Cell(x - 1, y - 1), Cell(x, y - 1), Cell(x + 1, y)), blockType)
    case T => Block(List(Cell(x, y), Cell(x - 1, y), Cell(x + 1, y), Cell(x, y - 1)), blockType)
    case Z => Block(List(Cell(x, y), Cell(x - 1, y), Cell(x, y - 1), Cell(x + 1, y - 1)), blockType)
  }
}

case class Block(cells: List[Cell], blockType: BlockType) {
  def isRotationAllowed(rotation: Move, board: Board): Boolean = {
    val rotated = makeRotation(rotation, board)
    rotated.cells forall { cell => (cell isInBorders board) && board.cellsByCoor(cell.x, cell.y).isEmpty }
  }

  def makeRotation(rotation: Move, board: Board): Block = {
    blockType match {
      case O => this
      case _ => val x = cells.head.x
        val y = cells.head.y
        val shifted = cells map { cell => Cell(cell.x - x, cell.y - y) }
        val rotated = shifted map { cell => Cell(cell.y + x, -cell.x + y) }
        Block(rotated, blockType)
    }
  }

  def makeMove(move: Move, board: Board): Block =
    new Block(cells map { _.makeMove(move, board) }, blockType)

  def isMoveAllowed(move: Move, board: Board): Boolean = cells.forall(_.isMoveAllowed(move, board))

  def lowest: Cell = cells minBy { _.y }

  def highest: Cell = cells minBy { _.y }

  def cellsByCoor(x: Int, y: Int): List[Cell] = cells.filter(cell => cell.x == x && cell.y == y)
}
