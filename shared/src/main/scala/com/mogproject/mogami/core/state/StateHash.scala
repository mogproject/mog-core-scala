package com.mogproject.mogami.core.state

import com.mogproject.mogami.core.Ptype.PAWN
import com.mogproject.mogami.core.move.Move
import com.mogproject.mogami.core.state.State.HandType
import com.mogproject.mogami.core.{Hand, Piece, Player, Square}

import scala.util.Random

/**
  * Hash numbers that determine a specific state
  */

object StateHash {
  type StateHash = Long

  private[this] val random = new Random(12345)

  /** player(2) */
  private[this] lazy val turnWhiteConstant: StateHash = Long.MinValue

  /** piece(32) x square(81) */
  private[this] lazy val boardConstants: Vector[StateHash] = Vector.fill(32 * 81)(random.nextLong() & Long.MaxValue)

  /** player(2) x numbers(18) */
  private[this] lazy val handPawnConstants: Vector[StateHash] = Vector.fill(2 * 18)(random.nextLong() & Long.MaxValue)

  /** player(2) x ptype(7) x numbers(4) */
  private[this] lazy val handConstants: Vector[StateHash] = Vector.fill(2 * 7 * 4)(random.nextLong() & Long.MaxValue)

  /**
    * @param player turn to move
    * @return hash constant
    */
  def get(player: Player): StateHash = if (player.isBlack) 0L else turnWhiteConstant

  /**
    * @param piece  piece
    * @param square square on board
    * @return hash constant
    */
  def get(piece: Piece, square: Square): StateHash = boardConstants(square.index << 5 | piece.id)

  /**
    * @param hand   hand
    * @param number between 1 and 18 if ptype == pawn else between 1 and 4, inclusive
    * @return hash constant
    */
  def get(hand: Hand, number: Int): StateHash =
    if (hand.ptype == PAWN)
      handPawnConstants((number - 1) << 1 | hand.owner.id)
    else
      handConstants(((hand.ptype.id - 9) << 1 | hand.owner.id) << 2 | (number - 1))

  /**
    * @param state state
    * @return hash constant
    */
  def get(state: State): StateHash = get(state.turn) ^
    state.board.foldLeft(0L) { case (x, (s, p)) => x ^ get(p, s) } ^
    state.hand.foldLeft(0L) { case (x, (h, n)) => (1 to n).foldLeft(x) { case (y, i) => y ^ get(h, i) } }

  /**
    * Get difference between before and after one move
    * @param hand hand
    * @param move move
    * @return difference of the hash values
    */
  def getDifference(hand: HandType, move: Move): StateHash = {
    (move.moveFrom match {
      case Left(from) => // move on board
        get(move.oldPiece, from) ^ move.captured.map { cap => //captured
          val capturedPiece = Piece(!move.player, cap)
          val capturedHand = Hand(move.player, cap.demoted)
          get(capturedPiece, move.to) ^ get(capturedHand, hand(capturedHand) + 1)
        }.getOrElse(0L)
      case Right(h) => // drop
        get(h, hand(h))
    }) ^ turnWhiteConstant ^ get(move.newPiece, move.to)
  }
}
