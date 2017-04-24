package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.io.sfen.{SfenFactory, SfenLike, UsenFactory, UsenLike}
import com.mogproject.mogami.core.state.State
import com.mogproject.mogami.util.Implicits._

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

/**
  *
  */

/**
  * Move builder for SFEN-formatted string
  */
sealed trait MoveBuilderSfen extends MoveBuilder with SfenLike with UsenLike {

  protected def makeUsenString(from: Int, to: Square, promote: Boolean): String = encodeBase36((from * 81 + to.index) * 2 + promote.fold(1, 0))

  private[this] def encodeBase36(x: Int): String = {
    require(0 <= x && x < 36 * 36 * 36)
    ("00" + java.lang.Long.toString(x, 36)).takeRight(3)
  }

}

object MoveBuilderSfen extends SfenFactory[MoveBuilderSfen] with UsenFactory[MoveBuilderSfen] {
  private[this] val patternOnBoard: Regex = """([1-9][a-i])([1-9][a-i])([+]?)""".r
  private[this] val patternInHand: Regex = """([PLNSGBR])[*]([1-9][a-i])""".r

  override def parseSfenString(s: String): MoveBuilderSfen = {
    s match {
      case patternOnBoard(from, to, promote) =>
        val f = Square.parseSfenString(from)
        val t = Square.parseSfenString(to)
        if (f == t) throw new RecordFormatException(1, s"move_to must not be the same as move_from: ${s}")
        MoveBuilderSfenBoard(f, t, promote == "+")
      case patternInHand(ptype, to) =>
        val p = Piece.parseSfenString(ptype)
        // already assured that ptype is in-hand type
        val t = Square.parseSfenString(to)
        MoveBuilderSfenHand(p.ptype, t)
      case _ => throw new RecordFormatException(1, s"invalid move format: ${s}")
    }
  }

  override def parseUsenString(s: String): MoveBuilderSfen = {
    Try(decodeBase36(s)) match {
      case Success(x) =>
        if (x < 0) throw new RecordFormatException(1, s"USEN string value cannot be negative: ${s}")

        val (a, b, c) = (x / (81 * 2), x / 2 % 81, x % 2)
        if (a >= 81) {
          Try(Ptype(a - 81)) match {
            case Success(pt) if pt.isHandType => MoveBuilderSfenHand(pt, Square(b))
            case _ => throw new RecordFormatException(1, s"invalid hand type: ${s}")
          }
        } else {
          if (a == b) throw new RecordFormatException(1, s"move_to must not be the same as move_from: ${s}")
          MoveBuilderSfenBoard(Square(a), Square(b), c == 1)
        }
      case Failure(_) =>
        throw new RecordFormatException(1, s"invalid move format: ${s}")
    }
  }

  private[this] def decodeBase36(s: String): Int = java.lang.Long.valueOf(s, 36).toInt

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

  override def toUsenString: String = makeUsenString(from.index, to, promote)

  override def toMove(state: State, lastMoveTo: Option[Square] = None, isStrict: Boolean = true): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      newPtype = promote.fold(oldPiece.ptype.promoted, oldPiece.ptype)
      isSame = lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype).filter(_ != KING)
      mv <- Try(Move(state.turn, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, None, isStrict)).toOption
    } yield mv
}

case class MoveBuilderSfenHand(ptype: Ptype, to: Square) extends MoveBuilderSfen {
  override def toSfenString: String = s"${Piece(Player.BLACK, ptype).toSfenString}*${to.toSfenString}"

  override def toUsenString: String = makeUsenString(ptype.id + 81, to, false)

  override def toMove(state: State, lastMoveTo: Option[Square] = None, isStrict: Boolean = true): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(state.turn, None, to, ptype, promote = false, isSameSquare = false, movement, None, isCheck, None, isStrict)).toOption
    } yield mv
  }
}
