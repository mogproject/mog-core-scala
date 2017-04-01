package com.mogproject.mogami.core.move


import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

import scala.util.Try


sealed trait MoveBuilderCsa extends MoveBuilder with CsaLike

object MoveBuilderCsa extends CsaFactory[MoveBuilderCsa] {
//  private[this] val pattern: Regex = """([^,]+)(?:,T([0-9]+))?""".r

  //  def parseTime(s: String): Option[(String, Option[Int])] = s match {
  //    case pattern(mv, null) => Some((mv, None))
  //    case pattern(mv, tm) => Try(tm.toInt).filter(_ >= 0).map(x => (mv, Some(x))).toOption
  //    case _ => None
  //  }

  def parseTime(nel: NonEmptyLines): (Line, Option[Int]) = nel.lines.toList match {
    case mv :: Nil => (mv, None)
    case mv :: (tm, n) :: Nil if tm.startsWith("T") =>
      Try(tm.tail.toInt).filter(_ >= 0).map(x => (mv, Some(x))).getOrElse {
        throw new RecordFormatException(n, s"invalid time expression: ${tm}")
      }
    case _ :: (ln, n) :: Nil => throw new RecordFormatException(n, s"invalid format: ${ln}")
    case _ :: _ :: (ln, n) :: _ => throw new RecordFormatException(n, s"too long expression: ${ln}")
    // $COVERAGE-OFF$
    case Nil => throw new RuntimeException("never happens")
    // $COVERAGE-ON$
  }


  override def parseCsaString(nel: NonEmptyLines): MoveBuilderCsa = {
    val ((mv, n), t) = parseTime(nel)
    if (mv.length != 7) throw new RecordFormatException(n, s"invalid move expression: ${mv}")

    def f(begin: Int, end: Int): NonEmptyLines = NonEmptyLines(n, mv.substring(begin, end))

    val isHand = mv.substring(1, 3) == "00"
    val pl = Player.parseCsaString(f(0, 1))
    val to = Square.parseCsaString(f(3, 5))
    val pt = Ptype.parseCsaString(f(5, 7))

    if (isHand) {
      if (!pt.isHandType) throw new RecordFormatException(n, s"invalid piece type for hand: ${mv}")
      MoveBuilderCsaHand(pl, to, pt, t)
    } else {
      val from = Square.parseCsaString(f(1, 3))
      if (from == to) throw new RecordFormatException(n, s"move_to must not be the same as move_from: ${mv}")
      MoveBuilderCsaBoard(pl, from, to, pt, t)
    }
  }


  //  override def parseCsaString(s: String): Option[MoveBuilderCsa] = {
  //    val isHand = s.slice(1, 3) == "00"
  //    for {
  //      (mv, t) <- parseTime(s)
  //      pl <- Player.parseCsaString(mv.substring(0, 1))
  //      from <- if (isHand) Some(Square(0)) else Square.parseCsaString(mv.substring(1, 3))
  //      to <- Square.parseCsaString(mv.substring(3, 5)) if isHand || to != from
  //      pt <- Ptype.parseCsaString(mv.substring(5)) if !isHand || pt.isHandType
  //    } yield {
  //      if (isHand) MoveBuilderCsaHand(pl, to, pt, t) else MoveBuilderCsaBoard(pl, from, to, pt, t)
  //    }
  //  }
}

case class MoveBuilderCsaBoard(player: Player, from: Square, to: Square, newPtype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = List(player, from, to, newPtype).map(_.toCsaString).mkString + timeToCsaString(elapsedTime)

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      promote = oldPiece.ptype != newPtype
      isSame = state.lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype).filter(_ != KING)
      mv <- Try(Move(player, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, elapsedTime, isStrict)).toOption
      if player == state.turn
      if oldPiece.ptype == promote.fold(newPtype.demoted, newPtype)
    } yield mv
}

case class MoveBuilderCsaHand(player: Player, to: Square, ptype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = s"${player.toCsaString}00${to.toCsaString}${ptype.toCsaString}${timeToCsaString(elapsedTime)}"

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(player, None, to, ptype, promote = false, isSameSquare = false, movement, captured = None, isCheck, elapsedTime, isStrict)).toOption
      if player == state.turn
    } yield mv
  }
}
