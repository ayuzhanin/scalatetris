package org.tetris.game.core

sealed abstract class Status

object Defeat extends Status

object Play extends Status

object Stopped extends Status