package com.mogproject.mogami.core

/**
  * Direction definitions
  */
object Direction extends Enumeration {

  sealed abstract class Direction(val id: Int)

  case object Forward extends Direction(0)

  case object DiagonallyForward extends Direction(1)

  case object Side extends Direction(2)

  case object DiagonallyBackward extends Direction(3)

  case object Backward extends Direction(4)

  case object KnightMove extends Direction(5)

  case object NoRelation extends Direction(6)

}

case class Displacement(direction: Direction.Direction, distance: Int)
