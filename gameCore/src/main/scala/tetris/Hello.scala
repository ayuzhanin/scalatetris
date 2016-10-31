package tetris

/**
  * Created by one on 26.10.2016.
  */
object Hello extends App {
  val height = 15
  val width = 15
  var gameState = GameState(Block(width / 2, height - 1, L), Board(Nil, height, width))

  val game = new Game(gameState, Player("one", HumanPlayer()))
  game.start()
}


