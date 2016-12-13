package com.mogproject.mogami.core.attack

import com.mogproject.mogami.core.Ptype.{GOLD, KING, KNIGHT, PAWN, SILVER}
import com.mogproject.mogami.core.{BitBoard, Piece, Ptype, Square}

/**
  *
  */
trait DirectAttack {
  def getDirectAttack(piece: Piece, square: Square): BitBoard =
    baseAttack.getOrElse(piece.ptype, baseAttack(GOLD)).flipByPlayer(piece.owner).shiftRight(5 - square.file).shiftUp(5 - square.rank)

  private[this] val baseAttack: Map[Ptype, BitBoard] = (Seq(PAWN, KNIGHT, SILVER, GOLD, KING) zip BitBoard.seq(
    """
      |--------- --------- --------- --------- ---------
      |--------- --------- --------- --------- ---------
      |--------- ---*-*--- --------- --------- ---------
      |----*---- --------- ---***--- ---***--- ---***---
      |--------- --------- --------- ---*-*--- ---*-*---
      |--------- --------- ---*-*--- ----*---- ---***---
      |--------- --------- --------- --------- ---------
      |--------- --------- --------- --------- ---------
      |--------- --------- --------- --------- ---------
    """.stripMargin)).toMap
}
