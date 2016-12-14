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
    * @param square    square
    * @param allOcc    occupancy bitboard of all pieces on board
    * @param myOcc     occupancy bitboard of pieces on board owned by the turn-to-move
    * @param myPawnOcc occupancy bitboard of pawns on board owned by the turn-to-move
    * @return attack bitboard
    */
  def get(piece: => Piece, square: => Square, allOcc: => BitBoard, myOcc: => BitBoard, myPawnOcc: => BitBoard): BitBoard =
    if (square.isHand)
      getDropAttack(piece, myPawnOcc) & ~allOcc
    else
      piece.isRanged.fold(getRangedAttack(piece, square, allOcc), getDirectAttack(piece, square)) & ~myOcc

}
