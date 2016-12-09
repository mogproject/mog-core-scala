package com.mogproject.mogami.core

/**
  * Direction definitions
  */
object Direction extends Enumeration {
  type Direction = Value
  val Forward, DiagonallyForward, Side, DiagonallyBackward, Backward, KnightMove, NoRelation = Value
}

case class Displacement(direction: Direction.Direction, distance: Int)
