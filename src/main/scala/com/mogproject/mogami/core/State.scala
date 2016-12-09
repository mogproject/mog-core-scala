package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.MapUtil

import scala.collection.mutable.ArrayBuffer

/**
  * State class
  */
case class State(turn: Player, board: Map[Square, Piece], hand: Map[Piece, Int]) extends CsaLike with SfenLike {

  import com.mogproject.mogami.core.State.PromotionFlag.{PromotionFlag, CannotPromote, CanPromote, MustPromote}

  override def toCsaString = {
    val s = new StringBuilder
    val buf = ArrayBuffer.fill(81)(" * ")
    board.foreach { case (sq, piece) => buf((sq.rank - 1) * 9 + sq.file - 1) = piece.toCsaString }
    s ++= (0 until 9).map { i =>
      (0 until 9).map { j => buf(i * 9 + 8 - j) }.mkString(s"P${i + 1}", "", "\n")
    }.mkString
    s ++= List(BLACK, WHITE).map { t =>
      "P" + t.toCsaString + Ptype.inHand.map { p =>
        ("00" + p.toCsaString) * hand.getOrElse(Piece(t, p), 0)
      }.mkString("", "", "\n")
    }.mkString
    s ++= turn.toCsaString
    s.result()
  }

  override def toSfenString = ???

  def getPromotionFlag(from: Square, to: Square): Option[PromotionFlag] = {
    if (from.isHand) {
      Some(CannotPromote)
    } else {
      for (p <- board.get(from) if p.owner == turn) yield {
        (p.ptype.isPromoted, from.isPromotionZone(turn) || to.isPromotionZone(turn), to.isLegalZone(p)) match {
          case (false, true, true) => CanPromote
          case (false, true, false) => MustPromote
          case _ => CannotPromote
        }
      }
    }
  }

  /**
    * Identify new piece from a move.
    *
    * @param move move instance
    * @return piece wrapped by Option
    */
  private[this] def getNewPiece(move: Move): Option[Piece] = move match {
    case Move(_, _, Some(player), Some(newPtype), _) => Some(Piece(player, newPtype))
    case Move(Square.HAND, _, _, Some(newPtype), _) => Some(Piece(turn, newPtype))
    case Move(from, _, _, _, Some(false)) => board.get(from)
    case Move(from, _, _, _, Some(true)) => board.get(from).map(_.promoted)
    case _ => None
  }

  /**
    * Get the square where the turn-to-move player's king.
    *
    * @return None if the king is not on board
    */
  def getKingSquare: Option[Square] = board.view.filter{case (s, p) => p == Piece(turn, KING)}.map(_._1).headOption

  /**
    * Check if the move is legal.
    *
    * @param move move to test
    * @return true if the move is legal
    */
  def isValidMove(move: Move): Boolean = {
    (for {
      _ <- verifyPlayer(move)
      _ <- if (move.from.isHand) verifyHandMove(move) else verifyBoardMove(move)
      newBoard = (board - move.from).updated(move.to, Piece(turn, PPAWN)) // put a dummy piece
      if verifyKing(newBoard, getKingSquare)
    } yield {}).isDefined
  }

  private[this] def verifyPlayer(move: Move): Option[Unit] = if (move.player.contains(!turn)) None else Some({})

  // sub methods for hand move
  private[this] def verifyHandMove(move: Move): Option[Unit] = {
    for {
      p <- move.newPtype.map(Piece(turn, _))
      if hand.get(p).exists(_ > 0)
      if board.get(move.to).isEmpty
      if move.to.isLegalZone(p)
      if verifyNifu(move)
    } yield {}
  }

  private[this] def verifyNifu(move: Move): Boolean =
    !move.newPtype.contains(PAWN) || !(1 to 9).map(Square(move.to.file, _)).exists(s => board.get(s).contains(Piece(turn, PAWN)))

  // sub methods for board move
  private[this] def verifyBoardMove(move: Move): Option[Unit] = {
    for {
      oldPiece <- board.get(move.from)
      if verifyNewPtype(oldPiece, move)
      if State.canAttack(board, move.from, move.to)
    } yield {}
  }

  private[this] def verifyNewPtype(oldPiece: Piece, move: Move): Boolean = move.newPtype match {
    case Some(np) if oldPiece == Piece(turn, np) => !move.promote.contains(true)
    case Some(np) if oldPiece == Piece(turn, np).demoted => !move.promote.contains(false)
    case None => !move.promote.contains(true) || oldPiece.promoted != oldPiece
  }

  // test if king is safe
  private[this] def verifyKing(newBoard: Map[Square, Piece], kingSquare: Option[Square]): Boolean = {
    kingSquare.forall { k =>
      newBoard.forall { case (s, p) =>
        p.owner == turn || !State.canAttack(newBoard, s, k)
      }
    }
  }

  /**
    * Make one move.
    *
    * @param move move to make
    * @return new state
    */
  def makeMove(move: Move): Option[State] = {
    val newPiece = getNewPiece(move)

    val releaseHand: Map[Piece, Int] => Map[Piece, Int] = h =>
      (for {p <- newPiece if move.from.isHand; n <- h.get(p)} yield h.updated(p, n - 1)).getOrElse(h)

    val obtainHand: Map[Piece, Int] => Map[Piece, Int] = h =>
      (for {p <- board.get(move.to); c = !p.demoted; n <- h.get(c)} yield h.updated(c, n + 1)).getOrElse(h)

    if (isValidMove(move))
      newPiece.map { p => State(!turn, (board - move.from).updated(move.to, p), (releaseHand andThen obtainHand) (hand)) }
    else
      None
  }

  def getPieceCount: Map[Piece, Int] = MapUtil.mergeMaps(board.groupBy(_._2).mapValues(_.size), hand)(_ + _, 0)

  def getUsedPtypeCount: Map[Ptype, Int] = getPieceCount.groupBy(_._1.ptype.demoted).mapValues(_.values.sum)

  def getUnusedPtypeCount: Map[Ptype, Int] = MapUtil.mergeMaps(State.capacity, getUsedPtypeCount)(_ - _, 0)

  def checkCapacity: Boolean = getPieceCount.filterKeys(_.ptype == KING).forall(_._2 <= 1) && getUnusedPtypeCount.values.forall(_ >= 0)

}

object State extends CsaStateReader with SfenStateReader with CsaFactory[State] with SfenFactory[State] {

  object PromotionFlag extends Enumeration {
    type PromotionFlag = Value
    val CannotPromote, CanPromote, MustPromote = Value
  }

  val EMPTY_HANDS: Map[Piece, Int] = (for (t <- Player.constructor; pt <- Ptype.inHand) yield Piece(t, pt) -> 0).toMap

  val empty = State(BLACK, Map.empty, EMPTY_HANDS)
  val capacity: Map[Ptype, Int] = Map(PAWN -> 18, LANCE -> 4, KNIGHT -> 4, SILVER -> 4, GOLD -> 4, BISHOP -> 2, ROOK -> 2, KING -> 2)

  def canAttack(board: Map[Square, Piece], from: Square, to: Square): Boolean = {
    (for {
      p <- board.get(from)
      (relation, distance) = from.getRelation(p.owner, to)
      if p.ptype.canMoveTo(relation, distance)  // check capability
      if from.getInnerSquares(to).toSet.intersect(board.keySet).isEmpty  // check blocking pieces
    } yield {}).isDefined
  }

  val HIRATE = State(BLACK, Map(
    Square(1, 1) -> Piece(WHITE, LANCE),
    Square(2, 1) -> Piece(WHITE, KNIGHT),
    Square(3, 1) -> Piece(WHITE, SILVER),
    Square(4, 1) -> Piece(WHITE, GOLD),
    Square(5, 1) -> Piece(WHITE, KING),
    Square(6, 1) -> Piece(WHITE, GOLD),
    Square(7, 1) -> Piece(WHITE, SILVER),
    Square(8, 1) -> Piece(WHITE, KNIGHT),
    Square(9, 1) -> Piece(WHITE, LANCE),
    Square(2, 2) -> Piece(WHITE, BISHOP),
    Square(8, 2) -> Piece(WHITE, ROOK),
    Square(1, 3) -> Piece(WHITE, PAWN),
    Square(2, 3) -> Piece(WHITE, PAWN),
    Square(3, 3) -> Piece(WHITE, PAWN),
    Square(4, 3) -> Piece(WHITE, PAWN),
    Square(5, 3) -> Piece(WHITE, PAWN),
    Square(6, 3) -> Piece(WHITE, PAWN),
    Square(7, 3) -> Piece(WHITE, PAWN),
    Square(8, 3) -> Piece(WHITE, PAWN),
    Square(9, 3) -> Piece(WHITE, PAWN),
    Square(1, 7) -> Piece(BLACK, PAWN),
    Square(2, 7) -> Piece(BLACK, PAWN),
    Square(3, 7) -> Piece(BLACK, PAWN),
    Square(4, 7) -> Piece(BLACK, PAWN),
    Square(5, 7) -> Piece(BLACK, PAWN),
    Square(6, 7) -> Piece(BLACK, PAWN),
    Square(7, 7) -> Piece(BLACK, PAWN),
    Square(8, 7) -> Piece(BLACK, PAWN),
    Square(9, 7) -> Piece(BLACK, PAWN),
    Square(2, 8) -> Piece(BLACK, ROOK),
    Square(8, 8) -> Piece(BLACK, BISHOP),
    Square(1, 9) -> Piece(BLACK, LANCE),
    Square(2, 9) -> Piece(BLACK, KNIGHT),
    Square(3, 9) -> Piece(BLACK, SILVER),
    Square(4, 9) -> Piece(BLACK, GOLD),
    Square(5, 9) -> Piece(BLACK, KING),
    Square(6, 9) -> Piece(BLACK, GOLD),
    Square(7, 9) -> Piece(BLACK, SILVER),
    Square(8, 9) -> Piece(BLACK, KNIGHT),
    Square(9, 9) -> Piece(BLACK, LANCE)
  ), EMPTY_HANDS)
}