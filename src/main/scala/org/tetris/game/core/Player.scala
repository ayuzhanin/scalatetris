package org.tetris.game.core


class Player(val name: String, val move: () => Move ) {
  def makeMove(gameState: GameState): GameState = {
    val next = move()
    gameState.makeMove(next)
  }
}