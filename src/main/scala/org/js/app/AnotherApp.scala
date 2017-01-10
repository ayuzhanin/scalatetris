package org.js.app

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.tetris.game.core._

import scala.scalajs.js.annotation.JSExport

@JSExport
class AnotherApp {
  @JSExport
  def main(): Unit = {

  }

  def init() = {
    val height = 24
    val width = 12
    val gameState = GameState(Block(width / 2, height + 1, L), Board(Nil, height, width), Play)

    // Handle keyboard controls
    var move: Move = null
    var updated = false

    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      updated = true
      move = e.keyCode match {
        case 40 => Down()
        case 37 => Left()
        case 39 => Right()
        case _ => Rotate()
      }
    }, false)

    def currentMove(): Move = {
      while (!updated){}
      move
    }

    val player = new Player("one", currentMove)

    val game = new Game(gameState, player, ???)
    game.start()

  }

}
