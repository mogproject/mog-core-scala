package com.mogproject

package object mogami {

  type Player = com.mogproject.mogami.core.Player
  val Player = com.mogproject.mogami.core.Player

  val BLACK = com.mogproject.mogami.core.Player.BLACK
  val WHITE = com.mogproject.mogami.core.Player.WHITE

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

  type State = com.mogproject.mogami.core.state.State
  val State = com.mogproject.mogami.core.state.State

  type StateHash = com.mogproject.mogami.core.state.StateHash.StateHash
  val StateHash = com.mogproject.mogami.core.state.StateHash

  type BoardType = com.mogproject.mogami.core.state.State.BoardType
  type HandType = com.mogproject.mogami.core.state.State.HandType
  type MoveFrom = com.mogproject.mogami.core.state.State.MoveFrom

  type MoveBuilderCsa = com.mogproject.mogami.core.move.MoveBuilderCsa
  val MoveBuilderCsa = com.mogproject.mogami.core.move.MoveBuilderCsa

  type MoveBuilderSfen = com.mogproject.mogami.core.move.MoveBuilderSfen
  val MoveBuilderSfen = com.mogproject.mogami.core.move.MoveBuilderSfen

  type MoveBuilderKif = com.mogproject.mogami.core.move.MoveBuilderKif
  val MoveBuilderKif = com.mogproject.mogami.core.move.MoveBuilderKif

  type MoveBuilderKi2 = com.mogproject.mogami.core.move.MoveBuilderKi2
  val MoveBuilderKi2 = com.mogproject.mogami.core.move.MoveBuilderKi2

  type Move = com.mogproject.mogami.core.move.Move
  val Move = com.mogproject.mogami.core.move.Move

  type SpecialMove = com.mogproject.mogami.core.move.SpecialMove
  val SpecialMove = com.mogproject.mogami.core.move.SpecialMove

  type Resign = com.mogproject.mogami.core.move.Resign
  val Resign = com.mogproject.mogami.core.move.Resign
  type IllegalMove = com.mogproject.mogami.core.move.IllegalMove
  val IllegalMove = com.mogproject.mogami.core.move.IllegalMove
  type TimeUp = com.mogproject.mogami.core.move.TimeUp
  val TimeUp = com.mogproject.mogami.core.move.TimeUp
  val Pause = com.mogproject.mogami.core.move.Pause

  type Game = com.mogproject.mogami.core.game.Game
  val Game = com.mogproject.mogami.core.game.Game

  type Branch = com.mogproject.mogami.core.game.Branch
  val Branch = com.mogproject.mogami.core.game.Branch

  type BranchNo = com.mogproject.mogami.core.game.Game.BranchNo

  type GamePosition = com.mogproject.mogami.core.game.Game.GamePosition
  val GamePosition = com.mogproject.mogami.core.game.Game.GamePosition

  type GameStatus = com.mogproject.mogami.core.game.GameStatus.GameStatus
  val GameStatus = com.mogproject.mogami.core.game.GameStatus

  type GameInfo = com.mogproject.mogami.core.game.GameInfo
  val GameInfo = com.mogproject.mogami.core.game.GameInfo

}

