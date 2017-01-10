package org.tetris.game.core

/**
  * Created by one on 06.01.2017.
  */
sealed abstract class Move(val spanX: Int = 0, val spanY: Int)

case class Down(override val spanX: Int = 0, override val spanY: Int = -1) extends Move(spanX, spanY)

case class Right(override val spanX: Int = 1, override val spanY: Int = 0) extends Move(spanX, spanY)

case class Left(override val spanX: Int = -1, override val spanY: Int = 0) extends Move(spanX, spanY)

case class Rotate() extends Move(0, 0)
