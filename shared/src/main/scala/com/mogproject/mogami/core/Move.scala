package com.mogproject.mogami.core

import com.mogproject.mogami._
import com.mogproject.mogami.core.Movement._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

import scala.util.Try
import scala.util.matching.Regex

/**
  * Move builder for CSA-formatted string
  */
trait MoveBuilder {
  protected def isCheckMove(state: State, from: Option[Square], to: Square, newPtype: Ptype): Boolean = {
    state.getKing(!state.turn).exists { k =>
      val occ = state.occupancy.set(to)
      val newOccAll = from.map(occ.reset).getOrElse(occ)
      val directAttack = Attack.get(Piece(state.turn, newPtype), Some(to), newOccAll, BitBoard.empty).get(k)
      val rangedAttack = from.exists { fr =>
        state.board.exists {
          case (s, p) if p.owner == state.turn && p.isRanged && fr != s => Attack.get(p, Some(s), newOccAll, BitBoard.empty).get(k)
          case _ => false
        }
      }
      directAttack || rangedAttack
    }
  }

  /**
    * Get set of squares from which the same ptype can move to the same destinatiton.
    */
  protected def getAmbiguousPieces(state: State, from: Option[Square], to: Square, oldPtype: Ptype): Set[Square] = {
    state.board
      .filter(_._2 == Piece(state.turn, oldPtype))
      .keySet.filter(fr => !from.contains(fr) && state.attackBBOnBoard(state.turn)(fr).get(to))
  }

  protected def getRelation(player: Player, from: Square, to: Square): (Int, Int) = {
    val r = from.rank.compare(to.rank)
    val f = from.file.compare(to.file)
    player.isBlack.fold((r, f), (-r, -f))
  }

  protected def getMovement(state: State, from: Option[Square], to: Square, oldPtype: Ptype): Option[Movement] = {
    val ambs = getAmbiguousPieces(state, from, to, oldPtype)

    from match {
      case _ if ambs.isEmpty => None
      case None => Some(Dropped)
      case Some(fr) =>
        val rel = getRelation(state.turn, fr, to)
        val relations = ambs.map(getRelation(state.turn, _, to))
        if (!relations.exists(_._1 == rel._1)) rel._1 match {
          case 0 => Some(Horizontally)
          case 1 => Some(Upward)
          case -1 => Some(Downward)
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
        } else if (!relations.exists(_._2 == rel._2)) rel._2 match {
          case 0 if !List(BISHOP, ROOK).contains(oldPtype.demoted) => Some(Vertical)
          case 0 if relations.head._2 == -1 => Some(Rightwards) // relations must contain one another
          case 0 if relations.head._2 == 1 => Some(Leftwards) // relations must contain one another
          case 1 => Some(Rightwards)
          case -1 => Some(Leftwards)
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
        } else rel match {
          case (-1, -1) => Some(LeftDownward)
          case (-1, 1) => Some(RightDownward)
          case (0, -1) => Some(LeftHorizontally)
          case (0, 1) => Some(RightHorizontally)
          case (1, -1) => Some(LeftUpward)
          case (1, 1) => Some(RightUpward)
          case (_, 0) => Some(Vertical)
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
        }
    }

  }

  def toMove(state: State): Option[Move]
}

sealed trait MoveBuilderCsa extends MoveBuilder with CsaLike {
  protected def timeToCsaString(time: Option[Int]): String = time.map(",T" + _.toString).getOrElse("")
}

object MoveBuilderCsa extends CsaFactory[MoveBuilderCsa] {
  private[this] val pattern: Regex = """(.{7})(?:,T([0-9]+))?""".r

  private[this] def parseTime(s: String): Option[(String, Option[Int])] = s match {
    case pattern(mv, null) => Some((mv, None))
    case pattern(mv, tm) => Try(tm.toInt).filter(_ >= 0).map(x => (mv, Some(x))).toOption
    case _ => None
  }

  override def parseCsaString(s: String): Option[MoveBuilderCsa] = {
    val isHand = s.slice(1, 3) == "00"
    for {
      (mv, t) <- parseTime(s)
      pl <- Player.parseCsaString(mv.substring(0, 1))
      from <- if (isHand) Some(Square(0)) else Square.parseCsaString(mv.substring(1, 3))
      to <- Square.parseCsaString(mv.substring(3, 5)) if isHand || to != from
      pt <- Ptype.parseCsaString(mv.substring(5)) if !isHand || pt.isHandType
    } yield {
      if (isHand) MoveBuilderCsaHand(pl, to, pt, t) else MoveBuilderCsaBoard(pl, from, to, pt, t)
    }
  }
}

case class MoveBuilderCsaBoard(player: Player, from: Square, to: Square, newPtype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = List(player, from, to, newPtype).map(_.toCsaString).mkString + timeToCsaString(elapsedTime)

  override def toMove(state: State): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      promote = oldPiece.ptype != newPtype
      isSame = state.lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype)
      mv <- Try(Move(player, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, elapsedTime)).toOption
      if player == state.turn
    } yield mv
}

case class MoveBuilderCsaHand(player: Player, to: Square, ptype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = s"${player.toCsaString}00${to.toCsaString}${ptype.toCsaString}${timeToCsaString(elapsedTime)}"

  override def toMove(state: State): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(player, None, to, ptype, promote = false, isSameSquare = false, movement, captured = None, isCheck, elapsedTime)).toOption
      if player == state.turn
    } yield mv
  }
}

/**
  * Move builder for SFEN-formatted string
  */
sealed trait MoveBuilderSfen extends MoveBuilder with SfenLike {
}

object MoveBuilderSfen extends SfenFactory[MoveBuilderSfen] {
  private[this] val patternOnBoard: Regex = """([1-9][a-i])([1-9][a-i])([+]?)""".r
  private[this] val patternInHand: Regex = """([PLNSGBR])[*]([1-9][a-i])""".r

  override def parseSfenString(s: String): Option[MoveBuilderSfen] = {
    s match {
      case patternOnBoard(from, to, promote) =>
        for {
          f <- Square.parseSfenString(from)
          t <- Square.parseSfenString(to) if to != from
        } yield MoveBuilderSfenBoard(f, t, promote == "+")
      case patternInHand(ptype, to) =>
        for {
          p <- Piece.parseSfenString(ptype) // already assured that ptype is in-hand type
          t <- Square.parseSfenString(to)
        } yield MoveBuilderSfenHand(p.ptype, t)
      case _ => None
    }
  }

  def apply(from: MoveFrom, to: Square, promote: Boolean): MoveBuilderSfen = from match {
    case Left(sq) =>
      MoveBuilderSfenBoard(sq, to, promote)
    case Right(h) =>
      require(!promote, "promote must be false when dropping")
      MoveBuilderSfenHand(h.ptype, to)
  }
}

case class MoveBuilderSfenBoard(from: Square, to: Square, promote: Boolean) extends MoveBuilderSfen {
  override def toSfenString: String = s"${from.toSfenString}${to.toSfenString}${promote.fold("+", "")}"

  override def toMove(state: State): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      newPtype = promote.fold(oldPiece.ptype.promoted, oldPiece.ptype)
      isSame = state.lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype)
      mv <- Try(Move(state.turn, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, None)).toOption
    } yield mv
}

case class MoveBuilderSfenHand(ptype: Ptype, to: Square) extends MoveBuilderSfen {
  override def toSfenString: String = s"${Piece(Player.BLACK, ptype).toSfenString}*${to.toSfenString}"

  override def toMove(state: State): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(state.turn, None, to, ptype, promote = false, isSameSquare = false, movement, None, isCheck, None)).toOption
    } yield mv
  }
}

/**
  * Move with complete information
  */
case class Move(player: Player,
                from: Option[Square], // None if from hand
                to: Square,
                newPtype: Ptype,
                promote: Boolean,
                isSameSquare: Boolean, // true when `to` is same as the last move's `to`
                movement: Option[Movement], // None = no ambiguity
                captured: Option[Ptype],
                isCheck: Boolean,
                elapsedTime: Option[Int] = None
               ) extends CsaLike with SfenLike with KifLike {
  require(!isDrop || !promote, "promote must be false when dropping")
  require(!isDrop || captured.isEmpty, "captured must be None when dropping")
  require(from.exists(_.isPromotionZone(player)) || to.isPromotionZone(player) || !promote, "either from or to must be in the promotion zone")
  require(from.map(_.getDisplacement(player, to)).forall(oldPtype.canMoveTo), "move must be within the capability")
  require(to.isLegalZone(newPiece), "to must be legal for the new piece")
  require(elapsedTime.forall(_ >= 0), "elapsedTime must be positive or zero")
  require(!captured.contains(KING), "king cannot be captured")
  require(oldPtype != PAWN || movement.isEmpty, "pawn cannot be ambiguous")

  def oldPtype: Ptype = if (promote) newPtype.demoted else newPtype

  def oldPiece: Piece = Piece(player, oldPtype)

  def newPiece: Piece = Piece(player, newPtype)

  def isDrop: Boolean = from.isEmpty

  def hasCapture: Boolean = captured.isDefined

  def capturedPiece: Option[Piece] = captured.map(Piece(!player, _))

  def moveFrom: MoveFrom = from.map(Left.apply).getOrElse(Right(Hand(player, oldPtype)))

  def couldPromote: Boolean = oldPtype.canPromote && from.exists(_.isPromotionZone(player) || to.isPromotionZone(player))

  override def toCsaString: String =
    from.map(fr => MoveBuilderCsaBoard(player, fr, to, newPtype, elapsedTime))
      .getOrElse(MoveBuilderCsaHand(player, to, newPtype, elapsedTime)).toCsaString

  override def toSfenString: String =
    from.map(fr => MoveBuilderSfenBoard(fr, to, promote)).getOrElse(MoveBuilderSfenHand(newPtype, to)).toSfenString

  override def toKifString: String =
    isSameSquare.fold("同", to.toKifString) + oldPtype.toKifString + movement.map(_.kifString).getOrElse("") + promote.fold("成", couldPromote.fold("不成", ""))
}

object Movement {

  abstract sealed class Movement(val kifString: String)

  case object Dropped extends Movement("打")

  case object Downward extends Movement("引")

  case object Horizontally extends Movement("寄")

  case object Upward extends Movement("上")

  case object Leftwards extends Movement("右")

  case object Rightwards extends Movement("左")

  case object Vertical extends Movement("直")

  case object LeftDownward extends Movement("右引")

  case object LeftHorizontally extends Movement("右寄")

  case object LeftUpward extends Movement("右上")

  case object RightDownward extends Movement("左引")

  case object RightHorizontally extends Movement("左寄")

  case object RightUpward extends Movement("左上")

}