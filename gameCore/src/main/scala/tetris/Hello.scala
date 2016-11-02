package tetris

/**
  * Created by one on 26.10.2016.
  */
object Hello extends App {
  val height = 10
  val width = 12
  var gameState = GameState(Block(width / 2, height + 1, L), Board(Nil, height, width), Play)

  val game = new Game(gameState, Player("one", HumanPlayer))
  game.start()
}


