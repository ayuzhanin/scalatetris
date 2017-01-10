package org.tetris.game.core

case class GameState(block: Block, board: Board, status: Status){
  private def randomBlock = {
    val x = board.width / 2
    val y = board.height + 1
    scala.util.Random.nextInt(7000) % 7 match {
      case 0 => Block(x, y, I)
      case 1 => Block(x, y, J)
      case 2 => Block(x, y, L)
      case 3 => Block(x, y, O)
      case 4 => Block(x, y, S)
      case 5 => Block(x, y, T)
      case 6 => Block(x, y, Z)
    }
  }

  // надо поправить (клетки должны быть соседними)
  def isDefeated: Boolean = {
    val boardHighestCell = board.highest
    val blockHighestCell = block.highest
    val blockLowestCell = block.lowest
    val neighbours = block.cells.flatMap(board.neighbours).toSet
    boardHighestCell.y + 1 == blockLowestCell.y && blockHighestCell.y >= board.height - 1 && (neighbours exists { _.y == boardHighestCell.y })
  }

  def makeMove(action: Move): GameState = action match {
    case Down(_, _) | Right(_, _) | Left(_, _) =>
      if (isDefeated) GameState(block, board, Defeat)
      else if (block.isMoveAllowed(action, board)) GameState(block.makeMove(action, board), board, status)
      else if (block.isMoveAllowed(Down(), board)) GameState(block, board, status)
      else GameState(randomBlock, board.consume(block).clear.pushDown, status)
    case Rotate() =>
      if (block.isRotationAllowed(action, board)) GameState(block.makeRotation(action, board), board, status)
      else GameState(block, board, status)
    case _ => this
  }

  override def toString: String = {
    def isCellEmpty(x: Int, y: Int) =
      if (board.cellsByCoor(x, y).isEmpty && block.cellsByCoor(x, y).isEmpty) " "
      else "X"

    val lines = (0 until board.height).reverse flatMap { y =>
      ((0 until board.width) map { x => isCellEmpty(x, y) }) :+ s"$y\n"
    }
    val boarders = ((0 to board.width) map { x => s"$x" } mkString " ") + "\n"
    val result = " " + boarders + " " + (lines mkString " ") + boarders
    result
  }
}