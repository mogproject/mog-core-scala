package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.MapUtil
import com.mogproject.mogami.util.BooleanOps.Implicits._
import com.mogproject.mogami.core.State.{BoardType, HandType}

/**
  * State class
  */
case class State(turn: Player, board: BoardType, hand: HandType) extends CsaLike with SfenLike {

  import com.mogproject.mogami.core.State.PromotionFlag.{PromotionFlag, CannotPromote, CanPromote, MustPromote}

  override def toCsaString: String = {
    val boardString = (1 to 9).map { rank =>
      (9 to 1 by -1).map { file => board.get(Square(file, rank)).map(_.toCsaString).getOrElse(" * ") }.mkString(s"P$rank", "", "")
    }.mkString("\n")

    val handString = Player.constructor.map { p =>
      s"P${p.toCsaString}" + Ptype.inHand.map { pt => s"00${pt.toCsaString}" * hand.getOrElse(Piece(p, pt), 0) }.mkString
    }.mkString("\n")

    Seq(boardString, handString, turn.toCsaString).mkString("\n")
  }

  override def toSfenString: String = {
    def stringifyNumber(n: Int, threshold: Int = 0): String = (n <= threshold).fold("", n.toString)

    val boardString = (1 to 9).map { rank =>
      val (ss, nn) = (9 to 1 by -1).map { file =>
        board.get(Square(file, rank))
      }.foldLeft(("", 0)) {
        case ((s, n), Some(p)) => (s + stringifyNumber(n) + p.toSfenString, 0)
        case ((s, n), None) => (s, n + 1)
      }
      ss + stringifyNumber(nn)
    }.mkString("/")

    val handString = hand.filter(_._2 != 0).toSeq.sorted.map { case (p, n) => stringifyNumber(n, 1) + p.toSfenString }.mkString

    s"$boardString ${turn.toSfenString} ${handString.isEmpty.fold("-", handString)}"
  }

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
    * Check if the move is legal.
    *
    * @param move move to test
    * @return true if the move is legal
    */
  def isValidMove(move: ExtendedMove): Boolean = {
    verifyPlayer(move) &&
      move.isDrop.fold(verifyHandMove(move), verifyBoardMove(move)) &&
      verifyKing(board - move.from + (move.to -> Piece(turn, PPAWN)))
  }

  private[this] def verifyPlayer(move: ExtendedMove): Boolean = move.player == turn

  // sub methods for hand move
  private[this] def verifyHandMove(move: ExtendedMove): Boolean =
    hand.get(move.newPiece).exists(_ > 0) && board.get(move.to).isEmpty && verifyNifu(move)

  private[this] def verifyNifu(move: ExtendedMove): Boolean =
    move.newPtype != PAWN || !(1 to 9).map(Square(move.to.file, _)).exists(s => board.get(s).contains(Piece(turn, PAWN)))

  // sub methods for board move
  private[this] def verifyBoardMove(move: ExtendedMove): Boolean =
    board.get(move.to).map(_.ptype) == move.captured && State.canAttack(board, move.from, move.to)

  // test if king is safe
  private[this] def verifyKing(newBoard: Map[Square, Piece]): Boolean = {
    State.getKingSquare(turn, newBoard).forall { k =>
      newBoard.forall { case (s, p) =>
        p.owner == turn || !State.canAttack(newBoard, s, k)
      }
    }
  }

  /** *
    * Check if the state is mated.
    *
    * @return true if mated
    */
  def isMated: Boolean = ???  // todo

  /**
    * Make one move.
    *
    * @param move move to make
    * @return new state
    */
  def makeMove(move: ExtendedMove): Option[State] = {
    if (isValidMove(move)) {
      val releaseHand: HandType => HandType = move.isDrop.when(MapUtil.decrementMap(_, move.newPiece))
      val obtainHand: HandType => HandType = h => move.capturedPiece.map(p => MapUtil.incrementMap(h, !p.demoted)).getOrElse(h)
      Some(State(!turn, board - move.from + (move.to -> move.newPiece), (releaseHand andThen obtainHand) (hand)))
    } else {
      None
    }
  }

  def getPieceCount: Map[Piece, Int] = MapUtil.mergeMaps(board.groupBy(_._2).mapValues(_.size), hand)(_ + _, 0)

  def getUsedPtypeCount: Map[Ptype, Int] = getPieceCount.groupBy(_._1.ptype.demoted).mapValues(_.values.sum)

  def getUnusedPtypeCount: Map[Ptype, Int] = MapUtil.mergeMaps(State.capacity, getUsedPtypeCount)(_ - _, 0)

  def checkCapacity: Boolean = getPieceCount.filterKeys(_.ptype == KING).forall(_._2 <= 1) && getUnusedPtypeCount.values.forall(_ >= 0)

}

object State extends CsaStateReader with SfenStateReader {

  type BoardType = Map[Square, Piece]
  type HandType = Map[Piece, Int]

  object PromotionFlag extends Enumeration {
    type PromotionFlag = Value
    val CannotPromote, CanPromote, MustPromote = Value
  }

  val EMPTY_HANDS: HandType = (for (t <- Player.constructor; pt <- Ptype.inHand) yield Piece(t, pt) -> 0).toMap

  val empty = State(BLACK, Map.empty, EMPTY_HANDS)
  val capacity: Map[Ptype, Int] = Map(PAWN -> 18, LANCE -> 4, KNIGHT -> 4, SILVER -> 4, GOLD -> 4, BISHOP -> 2, ROOK -> 2, KING -> 2)

  // TODO: use BitBoard
  def canAttack(board: Map[Square, Piece], from: Square, to: Square): Boolean = {
    (for {
      p <- board.get(from)
      if p.ptype.canMoveTo(from.getDisplacement(p.owner, to)) // check capability
      if from.getInnerSquares(to).toSet.intersect(board.keySet).isEmpty // check blocking pieces
    } yield {}).isDefined
  }

  /**
    * Get the square where the turn-to-move player's king.
    *
    * @return None if the king is not on board
    */
  def getKingSquare(player: Player, board: BoardType): Option[Square] =
    board.view.filter { case (s, p) => p == Piece(player, KING) }.map(_._1).headOption

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