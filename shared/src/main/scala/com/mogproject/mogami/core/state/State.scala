package com.mogproject.mogami.core.state

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move.{MoveBuilderCsaBoard, MoveBuilderCsaHand, MoveBuilderSfenBoard, MoveBuilderSfenHand}
import com.mogproject.mogami.core.{Hand, Player, Ptype}
import com.mogproject.mogami.util.Implicits._
import com.mogproject.mogami.util.MapUtil

import scala.util.Try


/**
  * State class
  */
case class State(turn: Player = BLACK,
                 board: BoardType = Map.empty,
                 hand: HandType = State.EMPTY_HANDS,
                 hint: Option[StateHint] = None
                ) extends CsaLike with SfenLike with KifLike with UsenLike with HtmlStateWriter {

  // if a hint is given, skip requirement checks
  if (hint.isEmpty) {
    require(checkCapacity, "the number of pieces must be within the capacity")
    require(hand.keySet == State.EMPTY_HANDS.keySet, "hand pieces must be in-hand type")
    require(board.forall { case (s, p) => s.isLegalZone(p) }, "all board pieces must be placed in their legal zones")
    require(!getKing(!turn).exists(getAttackBB(turn).get), "player must not be able to attack the opponent's king")
    require(!isNifu, "two pawns cannot be in the same file")
  }

  import com.mogproject.mogami.core.state.State.MoveFrom
  import com.mogproject.mogami.core.state.State.PromotionFlag.{CanPromote, CannotPromote, MustPromote, PromotionFlag}

  private[this] def checkCapacity: Boolean = {
    occupancy(Piece(BLACK, KING)).count <= 1 && occupancy(Piece(WHITE, KING)).count <= 1 && unusedPtypeCount.values.forall(_ >= 0)
  }

  /**
    * Practically unique hash value
    */
  lazy val hash: StateHash = hint.map(_.hash).getOrElse(StateHash.get(this))

  /**
    * Test if the board is Nifu.
    *
    * @return true if Nifu
    */
  private[this] def isNifu: Boolean = Player.constructor.exists { pl =>
    val files = board.withFilter(_._2 == Piece(pl, PAWN)).map(_._1.file).toSeq
    files.length != files.distinct.length
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: State => hash == that.hash
    case _ => false
  }

  override def hashCode(): Int = (turn.hashCode * 31 + board.hashCode) * 31 + hand.hashCode

  override def toCsaString: String = {
    val boardString = (1 to 9).map { rank =>
      (9 to 1 by -1).map { file => board.get(Square(file, rank)).map(_.toCsaString).getOrElse(" * ") }.mkString(s"P$rank", "", "")
    }.mkString("\n")

    val handString = Player.constructor.map { p =>
      s"P${p.toCsaString}" + Ptype.inHand.map { pt => s"00${pt.toCsaString}" * hand.getOrElse(Hand(p, pt), 0) }.mkString
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

    val handString = hand.filter(_._2 != 0).toSeq.sorted.map { case (p, n) => stringifyNumber(n, 1) + p.toPiece.toSfenString }.mkString

    s"$boardString ${turn.toSfenString} ${handString.isEmpty.fold("-", handString)}"
  }

  override def toUsenString: String = toSfenString.replace('/', '_').replace(' ', '.').replace('+', 'z')

  private[this] def numberToJapanese(n: Int): String = n match {
    case _ if 1 <= n && n <= 18 =>
      Vector("", "十")(n / 10) + Vector("", "一", "二", "三", "四", "五", "六", "七", "八", "九")(n % 10)
    case _ => ""
  }

  override def toKifString: String = {
    val handString = Player.constructor.map(pl => {
      val xs = hand.toSeq.sorted.flatMap {
        case (p, n) if p.owner == pl && n != 0 =>
          Seq(p.ptype.toJapaneseSimpleName + (n >= 2).fold(numberToJapanese(n), "") + "　")
        case _ => Seq()
      }
      xs.isEmpty.fold("なし", xs.mkString(""))
    })

    // todo: 上手/下手
    (Seq(
      "後手の持駒：" + handString(1),
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      (1 to 9).map { rank =>
        (9 to 1 by -1).map {
          file => board.get(Square(file, rank)).map(_.toKifString).getOrElse(" ・")
        }.mkString("|", "", s"|${numberToJapanese(rank)}")
      }.mkString("\n"),
      "+---------------------------+",
      "先手の持駒：" + handString(0)
    ) ++ turn.isBlack.fold(Seq.empty, Seq("後手番"))).mkString("\n")
  }

  def updateBoardPiece(square: Square, piece: Piece): Option[State] = Try(copy(board = board.updated(square, piece))).toOption

  def updateHandPiece(piece: Piece, num: Int): Option[State] = Try(copy(hand = hand.updated(Hand(piece), num))).toOption

  /**
    * Occupancy bitboards
    */
  private[this] lazy val occupancyAll: BitBoard = hint.map(_.occupancyAll).getOrElse(calculatedOccupancy._1)

  private[this] lazy val occupancyByOwner: Vector[BitBoard] = hint.map(_.occupancyByOwner).getOrElse(calculatedOccupancy._2)

  private[this] lazy val occupancyByPiece: Vector[BitBoard] = hint.map(_.occupancyByPiece).getOrElse(calculatedOccupancy._3)

  private[this] lazy val calculatedOccupancy: (BitBoard, Vector[BitBoard], Vector[BitBoard]) = {
    var occAll = BitBoard.empty
    val occOwn = Array.fill(2)(BitBoard.empty)
    val occPce = Array.fill(32)(BitBoard.empty)

    board.foreach { case (sq, p) =>
      occAll = occAll.set(sq)
      occOwn(p.owner.id) = occOwn(p.owner.id).set(sq)
      occPce(p.id) = occPce(p.id).set(sq)
    }
    (occAll, occOwn.toVector, occPce.toVector)
  }

  def occupancy: BitBoard = occupancyAll

  def occupancy(player: Player): BitBoard = occupancyByOwner(player.id)

  def occupancy(piece: Piece): BitBoard = occupancyByPiece(piece.id)

  def getSquares(piece: Piece): Set[Square] = occupancy(piece).toSet

  def getKing(player: Player): Option[Square] = occupancy(Piece(player, KING)).toList.headOption

  lazy val turnsKing: Option[Square] = getKing(turn)

  def getRangedPieces(player: Player): Seq[(Square, Piece)] = board.filter { case (_, p) => p.owner == player && p.isRanged }.toSeq

  /**
    * Attack bitboards of ALL on-board pieces
    */
  lazy val attackBBOnBoard: Map[Player, Map[Square, BitBoard]] = hint.map(_.attackBBOnBoard).getOrElse {
    val m = (for ((sq, piece@Piece(owner, _)) <- board) yield {
      (owner, sq) -> Attack.get(piece, Some(sq), occupancy, occupancy(Piece(owner, PAWN)))
    }).filter(_._2.nonEmpty).groupBy(_._1._1).mapValues(_.map { case ((_, s), b) => s -> b })

    Map(BLACK -> Map.empty[Square, BitBoard], WHITE -> Map.empty[Square, BitBoard]) ++ m
  }

  /**
    * Attack bitboards of the turn-to-move player's hands
    */
  lazy val attackBBInHand: Map[Hand, BitBoard] = for {
    (h, num) <- hand if h.owner == turn && num > 0
  } yield {
    h -> Attack.get(h.toPiece, None, occupancy, occupancy(Piece(turn, PAWN)))
  }

  def getAttackBB(player: Player): BitBoard = attackBBOnBoard(player).values.fold(BitBoard.empty)(_ | _)

  /**
    * Get the positions of pieces that are attacking the turn player's king
    *
    * @return set of squares
    */
  lazy val attackers: Set[Square] = turnsKing.map(k => attackBBOnBoard(!turn).filter(_._2.get(k)).keys.toSet).getOrElse(Set.empty)

  /**
    * Get the guard pieces, which protect the turn player's king from ranged attack.
    *
    * @return set of squares and guarding area + attacker bitboards
    */
  lazy val guards: Map[Square, BitBoard] = {
    for {
      (s, p) <- board if p.owner == !turn && p.isRanged
      k <- getKing(turn)
      bt = s.getBetweenBB(k) if Attack.getRangedAttack(p, s, BitBoard.empty).get(k)
      g = bt & occupancy if g.count == 1
    } yield {
      g.toList.head -> bt.set(s)
    }
  }

  /**
    * Check if the player is checked.
    */
  lazy val isChecked: Boolean = turnsKing.exists(getAttackBB(!turn).get)

  def getNonSuicidalMovesOnBoard: Map[Square, BitBoard] = for ((sq, bb) <- attackBBOnBoard(turn)) yield {
    if (board(sq).ptype == KING)
      sq -> (bb & ~getAttackBB(!turn))
    else if (guards.keySet.contains(sq))
      sq -> (bb & guards(sq))
    else
      sq -> bb
  }

  def getEscapeMoves: Map[MoveFrom, BitBoard] = {
    require(turnsKing.isDefined)

    // king's move
    val king = turnsKing.get
    val occExceptKing = occupancyAll.reset(king)
    val attackerPotentialBB: BitBoard = attackers.map(sq => Attack.get(board(sq), Some(sq), occExceptKing, BitBoard.empty)).fold(BitBoard.empty)(_ | _)
    val kingEscape = Map(king -> (attackBBOnBoard(turn)(king) & ~(getAttackBB(!turn) | occupancy(turn) | attackerPotentialBB)))

    // move a piece between king and the attacker or capture the attacker (except king's move)
    val attacker = if (attackers.size == 1) attackers.headOption else None
    val between = attacker.map(king.getBetweenBB)
    val betweenAndAttacker = attacker.map(atk => between.get.set(atk))

    val moveBetween = for {
      (sq, bb) <- getNonSuicidalMovesOnBoard if sq != king
      bt <- betweenAndAttacker
    } yield sq -> (bb & bt)

    // drop a piece between king and the attacker
    val dropBetween = for ((sq, bb) <- attackBBInHand; bt <- between) yield sq -> (bb & bt)

    (kingEscape ++ moveBetween).map { case (k, v) => Left(k) -> v } ++ dropBetween.map { case (k, v) => Right(k) -> v }
  }

  /**
    * All legal moves in the bitboard description
    *
    * @return map of the square from MoveFrom and attack bitboard
    */
  lazy val legalMovesBB: Map[MoveFrom, BitBoard] = {
    val m: Map[MoveFrom, BitBoard] = if (isChecked)
      getEscapeMoves
    else
      getNonSuicidalMovesOnBoard.map { case (k, v) => Left(k) -> v } ++ attackBBInHand.map { case (k, v) => Right(k) -> v }
    m.mapValues(_ & ~occupancy(turn)).filter(_._2.nonEmpty)
  }

  /**
    * Get all legal moves.
    *
    * @param lastMoveTo last move to
    * @note This method can be relatively expensive.
    * @return vector of legal moves
    */
  def legalMoves(lastMoveTo: Option[Square]): Vector[Move] = (
    for {
      (from, bb) <- legalMovesBB
      to <- bb.toList
      promote <- getPromotionList(from, to)
      mv <- from.fold(MoveBuilderSfenBoard(_, to, promote), p => MoveBuilderSfenHand(p.ptype, to)).toMove(this, lastMoveTo)
    } yield mv).toVector

  /** *
    * Check if the state is mated.
    *
    * @return true if mated
    */
  def isMated: Boolean = legalMovesBB.isEmpty

  // functions for makeMove
  private[this] def releaseBoard(move: Move): BoardType => BoardType = move.from.when(sq => b => b - sq)

  private[this] def releaseHand(move: Move): HandType => HandType = move.isDrop.when(MapUtil.decrementMap(_, Hand(move.newPiece)))

  private[this] def obtainHand(move: Move): HandType => HandType = move.capturedPiece.when(p => h => MapUtil.incrementMap(h, Hand(!p.demoted)))

  /**
    * Make one move.
    *
    * @param move move to make
    * @return new state
    */
  def makeMove(move: Move): Option[State] = isValidMove(move).option {
    val (newBoard, newHand) = makeNextPosition(move)
    val newOccs = getUpdatedOccupancy(move)

    val hint = StateHint(
      StateHash.getNextStateHash(this, move),
      newOccs._1,
      newOccs._2,
      newOccs._3,
      unusedPtypeCount,
      getUpdatedAttackBBOnBoard(move, newOccs._1)
    )
    State(!turn, newBoard, newHand, Some(hint))
  }

  /**
    * Make the next board and hand position from a move
    *
    * @param move illegal moves are also acceptable
    * @return next board and hand
    */
  def makeNextPosition(move: Move): (BoardType, HandType) = {
    val newBoard = releaseBoard(move)(board) + (move.to -> move.newPiece)
    val newHand = (releaseHand(move) andThen obtainHand(move)) (hand)
    (newBoard, newHand)
  }

  private[this] def getUpdatedOccupancy(move: Move): (BitBoard, Vector[BitBoard], Vector[BitBoard]) = {
    var occAll = occupancyAll
    val occOwn = occupancyByOwner.toArray
    val occPce = occupancyByPiece.toArray

    move.from.foreach { fr =>
      occAll = occAll.reset(fr)
      occOwn(move.player.id) = occOwn(move.player.id).reset(fr)
      occPce(move.oldPiece.id) = occPce(move.oldPiece.id).reset(fr)
    }

    occAll = occAll.set(move.to)
    occOwn(move.player.id) = occOwn(move.player.id).set(move.to)
    occPce(move.newPiece.id) = occPce(move.newPiece.id).set(move.to)

    move.capturedPiece.foreach { cp =>
      occOwn(cp.owner.id) = occOwn(cp.owner.id).reset(move.to)
      occPce(cp.id) = occPce(cp.id).reset(move.to)
    }
    (occAll, occOwn.toVector, occPce.toVector)
  }

  private[this] def getUsedPtypeCount: Map[Ptype, Int] = {
    val a = board.values.map(_.ptype.demoted).foldLeft(Map.empty[Ptype, Int]) { case (m, pt) => MapUtil.incrementMap(m, pt) }
    hand.foldLeft(a) { case (m, (h, n)) => m.updated(h.ptype, m.getOrElse(h.ptype, 0) + n) }
  }

  private[this] def getUpdatedAttackBBOnBoard(move: Move, occAll: BitBoard): Map[Player, Map[Square, BitBoard]] = {
    val removeMoveFrom: Map[Square, BitBoard] => Map[Square, BitBoard] = m => if (move.isDrop) m else m - move.from.get
    val removeCaptured: Map[Square, BitBoard] => Map[Square, BitBoard] = m => if (move.hasCapture) m - move.to else m
    val addMoveTo: Map[Square, BitBoard] => Map[Square, BitBoard] = m => m.updated(move.to, Attack.get(move.newPiece, Some(move.to), occAll, BitBoard.empty))

    val affectedBB = move.from.foldLeft(BitBoard.ident(move.to)) { case (bb, sq) => bb.set(sq) }
    val initialMap = Map(
      turn -> (removeMoveFrom andThen addMoveTo) (attackBBOnBoard(turn)),
      !turn -> removeCaptured(attackBBOnBoard(!turn))
    )

    // re-calculate ranged pieces
    board.foldLeft(initialMap) {
      case (m, (sq, p)) if p.isRanged && !affectedBB.get(sq) && (attackBBOnBoard(p.owner)(sq) & affectedBB).nonEmpty =>
        m.updated(p.owner, m(p.owner).updated(sq, Attack.get(p, Some(sq), occAll, BitBoard.empty)))
      case (m, _) => m
    }
  }

  lazy val unusedPtypeCount: Map[Ptype, Int] = hint.map(_.unusedPtypeCount).getOrElse(MapUtil.mergeMaps(State.capacity, getUsedPtypeCount)(_ - _, 0))

  def canAttack(from: Square, to: Square): Boolean = canAttack(Left(from), to)

  def canAttack(from: MoveFrom, to: Square): Boolean = legalMovesBB.get(from).exists(_.get(to))

  /**
    * @note This method does not check the capability of the piece
    */
  def getPromotionFlag(from: MoveFrom, to: Square): Option[PromotionFlag] = {
    from match {
      case Left(fr) =>
        for (p <- board.get(fr) if p.owner == turn) yield {
          (p.ptype.canPromote, fr.isPromotionZone(turn) || to.isPromotionZone(turn), to.isLegalZone(p)) match {
            case (true, true, true) => CanPromote
            case (true, true, false) => MustPromote
            case _ => CannotPromote
          }
        }
      case Right(_) => Some(CannotPromote)
    }
  }

  private[this] def getPromotionList(from: MoveFrom, to: Square): List[Boolean] = getPromotionFlag(from, to) match {
    case Some(CanPromote) => List(false, true)
    case Some(MustPromote) => List(true)
    case Some(CannotPromote) => List(false)
    case None => List()
  }

  /**
    * Check if the move is legal.
    *
    * @param move move to test
    * @return true if the move is legal
    */
  def isValidMove(move: Move): Boolean = {
    val mf = move.moveFrom
    canAttack(mf, move.to) && getPromotionList(mf, move.to).contains(move.promote)
  }

  /**
    * Check if the in-hand piece is non-empty.
    */
  def hasHand(h: Hand): Boolean = hand.get(h).exists(_ > 0)

  /**
    * Check if Uchifuzume can happen at the next move.
    *
    * @return true if Uchifuzume can happen
    * @note condition:
    *       - Turn's player can drop a pawn in hand on the front square of the opponent's king
    *       - The opponent's king cannot move
    *       - The opponent's pieces do not protect the opponent's king's front square
    */
  def isUchifuzumePossible: Boolean = getKing(!turn).exists { opponentKing =>
    val kingsFrontRank = opponentKing.rank + turn.isBlack.fold(1, -1)
    (1 <= kingsFrontRank && kingsFrontRank <= 9) && {
      val kingsFront = Square(opponentKing.file, kingsFrontRank)

      attackBBInHand.get(Hand(turn, PAWN)).exists(_.get(kingsFront)) &&
        (attackBBOnBoard(!turn)(opponentKing) & ~getAttackBB(turn) & (~occupancy(!turn))).isEmpty &&
        attackBBOnBoard(!turn).forall { case (sq, bb) => sq == opponentKing || !bb.get(kingsFront) }
    }
  }

  /**
    * Create a Move instance from the next state
    *
    * @param nextState  next state
    * @param lastMoveTo last move-to
    * @return None if there is no valid move
    */
  def createMoveFromNextState(nextState: State, lastMoveTo: Option[Square] = None): Option[Move] = {
    val moveBuilder = ((board.keySet -- nextState.board.keySet).headOption, (nextState.board.toSet -- board.toSet).headOption) match {
      case (None, Some((to, newPiece))) => Some(MoveBuilderCsaHand(turn, to, newPiece.ptype))
      case (Some(from), Some((to, newPiece))) => Some(MoveBuilderCsaBoard(turn, from, to, newPiece.ptype))
      case _ => None
    }

    for {
      mb <- moveBuilder
      mv <- mb.toMove(this, lastMoveTo)
      nxt <- makeMove(mv)
      if nxt == nextState // verify
    } yield mv
  }
}

object State extends CsaStateReader with SfenStateReader with KifStateReader {

  type BoardType = Map[Square, Piece]
  type HandType = Map[Hand, Int]

  // workaround for IntelliJ IDEA
  override def parseSfenString(s: String): State = super.parseSfenString(s)

  override def parseUsenString(s: String): State = super.parseUsenString(s)

  // board or hand
  type MoveFrom = Either[Square, Hand]

  object PromotionFlag {

    sealed trait PromotionFlag

    case object CannotPromote extends PromotionFlag

    case object CanPromote extends PromotionFlag

    case object MustPromote extends PromotionFlag

  }

  val EMPTY_HANDS: HandType = (for (t <- Player.constructor; pt <- Ptype.inHand) yield Hand(t, pt) -> 0).toMap

  val empty = State(BLACK, Map.empty, EMPTY_HANDS)
  lazy val capacity: Map[Ptype, Int] = Map(PAWN -> 18, LANCE -> 4, KNIGHT -> 4, SILVER -> 4, GOLD -> 4, BISHOP -> 2, ROOK -> 2, KING -> 2)

  // constant states
  def HIRATE = StateConstant.HIRATE
  def MATING_BLACK = StateConstant.MATING_BLACK
  def MATING_WHITE = StateConstant.MATING_WHITE
  def HANDICAP_LANCE = StateConstant.HANDICAP_LANCE
  def HANDICAP_BISHOP = StateConstant.HANDICAP_BISHOP
  def HANDICAP_ROOK = StateConstant.HANDICAP_ROOK
  def HANDICAP_ROOK_LANCE = StateConstant.HANDICAP_ROOK_LANCE
  def HANDICAP_2_PIECE = StateConstant.HANDICAP_2_PIECE
  def HANDICAP_3_PIECE = StateConstant.HANDICAP_3_PIECE
  def HANDICAP_4_PIECE = StateConstant.HANDICAP_4_PIECE
  def HANDICAP_5_PIECE = StateConstant.HANDICAP_5_PIECE
  def HANDICAP_6_PIECE = StateConstant.HANDICAP_6_PIECE
  def HANDICAP_8_PIECE = StateConstant.HANDICAP_8_PIECE
  def HANDICAP_10_PIECE = StateConstant.HANDICAP_10_PIECE
  def HANDICAP_THREE_PAWNS = StateConstant.HANDICAP_THREE_PAWNS
  def HANDICAP_NAKED_KING = StateConstant.HANDICAP_NAKED_KING

}

/**
  * Inherits calculated information from the previous state so that computation time will be reduced.
  */
case class StateHint(hash: StateHash,
                     occupancyAll: BitBoard,
                     occupancyByOwner: Vector[BitBoard],
                     occupancyByPiece: Vector[BitBoard],
                     unusedPtypeCount: Map[Ptype, Int],
                     attackBBOnBoard: Map[Player, Map[Square, BitBoard]])
