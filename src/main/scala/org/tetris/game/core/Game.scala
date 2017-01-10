package org.tetris.game.core

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps


class Game(val initial: GameState, val one: Player, val renderGameState: Unit => Unit) {

  var gameState: GameState = initial
  val systemPlayer: Player = new Player("system", () => {
    Thread sleep 1000
    Down()
  })

  def updateGameState(player: Player): Unit = this.synchronized {
    if (gameState.status == Play) {
      gameState = player.makeMove(gameState)
    }
  }

  def play(player: Player): Future[Status] = Future {
    while (gameState.status == Play) {
      updateGameState(player)
      //println(gameState)
      renderGameState()
    }
    gameState.status
  }

  def start(): Unit = {
    val first: Future[Status] = play(one)
    val second: Future[Status] = play(systemPlayer)
    val confirmation: Status = Await.result(second, Duration.Inf)
    println(confirmation)
  }
}