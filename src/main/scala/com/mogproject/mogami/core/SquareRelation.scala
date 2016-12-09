package com.mogproject.mogami.core

/**
  *
  */
object SquareRelation extends Enumeration {
  type SquareRelation = Value
  val Forward, DiagonallyForward, Side, DiagonallyBackward, Backward, KnightMove, NoRelation = Value
}