package com.mogproject.mogami.core.game

/**
  * game status
  */
object GameStatus {

  sealed trait GameStatus

  case object Playing extends GameStatus

  case object Mated extends GameStatus

  case object Uchifuzume extends GameStatus

  case object PerpetualCheck extends GameStatus

  case object Drawn extends GameStatus

  case object TimedUp extends GameStatus

  case object IllegallyMoved extends GameStatus

  case object Resigned extends GameStatus

}
