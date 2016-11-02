package tetris

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Created by one on 25.10.2016.
  */

sealed abstract class Action(val spanX: Int = 0, val spanY: Int)

case class Down(override val spanX: Int = 0, override val spanY: Int = -1) extends Action(spanX, spanY)

case class Right(override val spanX: Int = 1, override val spanY: Int = 0) extends Action(spanX, spanY)

case class Left(override val spanX: Int = -1, override val spanY: Int = 0) extends Action(spanX, spanY)

case class Rotation() extends Action(0, 0)

case class Cell(x: Int, y: Int) {
  def isMoveAllowed(move: Action, board: Board): Boolean = {
    val nearestCells = board.cellsByCoor(x + move.spanX, y + move.spanY)
    val moved = Cell(x + move.spanX, y + move.spanY)
    moved.isInBorders(board) && nearestCells.isEmpty
  }

  def isInBorders(board: Board): Boolean =
    x >= 0 && x <= board.width- 1 && y >= 0 /*&& y <= board.height- 1*/

  def makeMove(move: Action, board: Board): Cell =
    if (isMoveAllowed(move, board)) Cell(x + move.spanX, y + move.spanY)
    else this
}

sealed abstract class BlockType

object I extends BlockType

object J extends BlockType

object L extends BlockType

object O extends BlockType

object S extends BlockType

object T extends BlockType

object Z extends BlockType

case class Block(cells: List[Cell], blockType: BlockType) {
  def isRotationAllowed(rotation: Action, board: Board): Boolean = {
    val rotated = makeRotation(rotation, board)
    rotated.cells forall { cell => (cell isInBorders board) && board.cellsByCoor(cell.x, cell.y).isEmpty }
  }

  def makeRotation(rotation: Action, board: Board): Block = {
    blockType match {
      case O => this
      case _ => val x = cells.head.x
        val y = cells.head.y
        val shifted = cells map { cell => Cell(cell.x - x, cell.y - y) }
        val rotated = shifted map { cell => Cell(cell.y + x, -cell.x + y) }
        Block(rotated, blockType)
    }
  }

  def makeMove(move: Action, board: Board): Block =
    new Block(cells map { _.makeMove(move, board) }, blockType)

  def isMoveAllowed(move: Action, board: Board): Boolean = cells forall {
    _.isMoveAllowed(move, board)
  }

  def lowest: Cell = cells minBy { _.y }

  def highest: Cell = cells minBy { _.y }

  def cellsByCoor(x: Int, y: Int): List[Cell] = cells filter { cell => cell.x == x && cell.y == y }
}

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

case class Board(cells: List[Cell], height: Int, width: Int) {
  def cellsByCoor(x: Int, y: Int): List[Cell] = cells filter { cell => cell.x == x && cell.y == y }

  def neighbours(cell: Cell): List[Cell] = cells filter {
    c =>
      ((c.x + 1 == cell.x || c.x - 1 == cell.x) && c.y == cell.y) ||
        (c.y + 1 == cell.y || c.y - 1 == cell.y) && c.x == cell.x
  }

  private def line(index: Int): List[Cell] = cells filter {
    _.y == index
  }

  def highest: Cell = cells match {
    case head::tail => cells maxBy( _.y )
    case Nil => Cell(0, 0)
  }

  def clear = Board(
    ((0 until height) map { line } filter { line => line.size != width }).flatten.toList,
    height,
    width
  )

  def consume(block: Block) = Board(block.cells ++ cells, height, width)

  private def isEmptyAt(lineIndex: Int): Boolean = !(cells exists { _.y == lineIndex })

  private def shiftDown(lineIndex: Int): Board =
    if (isEmptyAt(lineIndex - 1) && !isEmptyAt(lineIndex))
      Board(
        cells map { cell =>
        if (cell.y == lineIndex) cell.makeMove(Down(), this)
        else cell
      }, height, width)
    else this

  def pushDown: Board = {
    def shiftDownAll(board: Board): Board =
      (1 until height).foldLeft(board) { (board, index) => board shiftDown index }

    def isShiftAllowed(board: Board): Boolean =
      (1 until height) exists { index => board.isEmptyAt(index - 1) && !board.isEmptyAt(index) }

    def until(condition: Board => Boolean, action: Board => Board)(board: Board): Board =
      if (!condition(board)) board
      else until(condition, action)(action(board))

    until(isShiftAllowed, shiftDownAll)(this)
  }
}

sealed abstract class Status

object Defeat extends Status

object Play extends Status

object Stop extends Status

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

  def performAction(action: Action): GameState = action match {
    case Down(_, _) | Right(_, _) | Left(_, _) =>
      if (isDefeated) GameState(block, board, Defeat)
      else if (block.isMoveAllowed(action, board)) GameState(block.makeMove(action, board), board, status)
      else if (block.isMoveAllowed(Down(), board)) GameState(block, board, status)
      else GameState(randomBlock, board.consume(block).clear.pushDown, status)
    case Rotation() =>
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

sealed abstract class PlayerType
object SystemPlayer extends PlayerType
object HumanPlayer extends PlayerType

abstract class Player(val name: String, val playerType: PlayerType) {
  def actionCatcher: Action
  def play(gameState: GameState, action: Action) = {
      gameState.performAction(action)
  }
}

object Player {
  def apply(name: String, playerType: PlayerType): Player = playerType match {
    case SystemPlayer => new Player("system", playerType){
      override def actionCatcher: Action = Down()
    }
    case HumanPlayer => new Player(name, playerType){
      override def actionCatcher: Action = scala.io.StdIn.readLine() match {
        case "a" => Left()
        case "s" => Down()
        case "d" => Right()
        case _ => Rotation()
      }
    }
  }
}

class Game(val startingGameState: GameState, val one: Player){

  var gameState: GameState = startingGameState
  val systemPlayer: Player = Player("system", SystemPlayer)

  def updateGameState(player: Player, action: Action): Unit = this.synchronized {
    gameState = player.play(gameState, action)
  }

  def ifFunGetElseNull(fun: => Boolean, get: => Any): Option[Any] = Option {
    if (fun) get
    else null
  }

  def play(player: Player): Future[Status] = Future {
    while (gameState.status == Play) {
      val action = ifFunGetElseNull(gameState.status == Play, player.actionCatcher).asInstanceOf[Option[Action]]
      val playable = ifFunGetElseNull(gameState.status == Play, true).asInstanceOf[Option[Boolean]]
      for {p <- playable
           a <- action} updateGameState(player, a)
      player.playerType match {
        case SystemPlayer => Thread sleep 1000
        case _ =>
      }
      println(gameState)
    }
    gameState.status
  }

  def start(): Unit = {
    val first: Future[Status] = play(one)
    val second: Future[Status] = play(systemPlayer)
    val confirmation: Status = Await.result(first, Duration.Inf)
    println(confirmation)
  }
}