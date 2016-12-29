package com.mogproject.mogami.core.attack

import com.mogproject.mogami._

/**
  *
  */
trait DropAttack {
  def getDropAttack(piece: Piece, pawnOcc: => BitBoard): BitBoard = {
    (piece.ptype match {
      case PAWN => ~(BitBoard.rank1 | pawnOcc.spreadFiles)
      case LANCE => ~BitBoard.rank1
      case KNIGHT => ~(BitBoard.rank1 | BitBoard.rank2)
      case _ => BitBoard.full
    }).flipByPlayer(piece.owner)
  }
}
