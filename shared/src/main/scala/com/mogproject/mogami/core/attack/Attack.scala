package com.mogproject.mogami.core.attack

import com.mogproject.mogami._
import com.mogproject.mogami.util.Implicits._

/**
  *
  */
object Attack extends DropAttack with RangedAttack {
  /**
    * Get attack bitboard
    *
    * @param piece     piece
    * @param from      None when dropping
    * @param allOcc    occupancy bitboard of all pieces on board
    * @param myPawnOcc occupancy bitboard of pawns on board owned by the turn-to-move
    * @return attack bitboard
    */
  def get(piece: => Piece, from: => Option[Square], allOcc: => BitBoard, myPawnOcc: => BitBoard): BitBoard =
    from.map { fr =>
      piece.isRanged.fold(getRangedAttack(piece, fr, allOcc), getDirectAttack(piece, fr))
    }.getOrElse(getDropAttack(piece, myPawnOcc) & ~allOcc)
}
