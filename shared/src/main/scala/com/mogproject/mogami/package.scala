package com.mogproject

package object mogami {

  type Player = com.mogproject.mogami.core.Player
  val Player = com.mogproject.mogami.core.Player

  val BLACK = com.mogproject.mogami.Player.BLACK
  val WHITE = com.mogproject.mogami.Player.WHITE

  type Ptype = com.mogproject.mogami.core.Ptype
  val Ptype = com.mogproject.mogami.core.Ptype

  val PAWN = com.mogproject.mogami.core.Ptype.PAWN
  val LANCE = com.mogproject.mogami.core.Ptype.LANCE
  val KNIGHT = com.mogproject.mogami.core.Ptype.KNIGHT
  val SILVER = com.mogproject.mogami.core.Ptype.SILVER
  val GOLD = com.mogproject.mogami.core.Ptype.GOLD
  val BISHOP = com.mogproject.mogami.core.Ptype.BISHOP
  val ROOK = com.mogproject.mogami.core.Ptype.ROOK
  val KING = com.mogproject.mogami.core.Ptype.KING
  val PPAWN = com.mogproject.mogami.core.Ptype.PPAWN
  val PLANCE = com.mogproject.mogami.core.Ptype.PLANCE
  val PKNIGHT = com.mogproject.mogami.core.Ptype.PKNIGHT
  val PSILVER = com.mogproject.mogami.core.Ptype.PSILVER
  val PBISHOP = com.mogproject.mogami.core.Ptype.PBISHOP
  val PROOK = com.mogproject.mogami.core.Ptype.PROOK

  type Piece = com.mogproject.mogami.core.Piece
  val Piece = com.mogproject.mogami.core.Piece

  type Square = com.mogproject.mogami.core.Square
  val Square = com.mogproject.mogami.core.Square

  type Hand = com.mogproject.mogami.core.Hand
  val Hand = com.mogproject.mogami.core.Hand

  type BitBoard = com.mogproject.mogami.core.BitBoard
  val BitBoard = com.mogproject.mogami.core.BitBoard

  val Attack = com.mogproject.mogami.core.attack.Attack

  type State = com.mogproject.mogami.core.State
  val State = com.mogproject.mogami.core.State

  type BoardType = com.mogproject.mogami.core.State.BoardType
  type HandType = com.mogproject.mogami.core.State.HandType
  type MoveFrom = com.mogproject.mogami.core.State.MoveFrom

  type MoveBuilderCsa = com.mogproject.mogami.core.move.MoveBuilderCsa
  val MoveBuilderCsa = com.mogproject.mogami.core.move.MoveBuilderCsa

  type MoveBuilderSfen = com.mogproject.mogami.core.move.MoveBuilderSfen
  val MoveBuilderSfen = com.mogproject.mogami.core.move.MoveBuilderSfen

  type Move = com.mogproject.mogami.core.move.Move
  val Move = com.mogproject.mogami.core.move.Move

  type Game = com.mogproject.mogami.core.Game
  val Game = com.mogproject.mogami.core.Game

  type GameInfo = com.mogproject.mogami.core.GameInfo
  val GameInfo = com.mogproject.mogami.core.GameInfo

}

