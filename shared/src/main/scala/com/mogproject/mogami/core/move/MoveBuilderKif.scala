package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

import scala.util.matching.Regex
import scala.util.{Success, Try}

/**
  *
  */
sealed trait MoveBuilderKif extends MoveBuilder with KifLike


object MoveBuilderKif extends KifFactory[MoveBuilderKif] {
  private[this] val patternTime: Regex = """([^ ]+)(?:[ ]+[(][ ]*(\d+):[ ]*(\d+)[/](?:[ ]*(\d+):[ ]*(\d+):[ ]*(\d+))?[)])?[+]?""".r
  private[this] val pattern: Regex = """(.[　一二三四五六七八九]?)([成]?.)([成打]?)(?:[(]([1-9]{2})[)])?""".r

  def parseTime(line: Line): (Line, Option[Int]) = line match {
    case (patternTime(mv, null, null, null, null, null), n) => ((mv, n), None)
    case (patternTime(mv, mm, ss, _, _, _), n) => (Try(mm.toInt), Try(ss.toInt)) match {
      case (Success(mx), Success(sx)) if mx <= Int.MaxValue / 60 && sx < 60 => ((mv, n), Some(mx * 60 + sx))
      case _ => throw new RecordFormatException(n, s"invalid time expression: mm=${mm}, ss=${ss}")
    }
    case (x, n) => throw new RecordFormatException(n, s"invalid time format: ${x}")
  }

  override def parseKifString(nel: NonEmptyLines): MoveBuilderKif = {
    if (nel.lines.length >= 2) {
      throw new RecordFormatException(nel.lines(1)._2, s"too long move expression: ${nel.lines(1)._1}")
    } else {
      val ((mv, n), t) = parseTime(nel.lines.head)
       mv match {
        case pattern(toStr, ptStr, "打", null) =>
          val to = Square.parseKifString(NonEmptyLines(n, toStr))
          val pt = Ptype.parseKifString(NonEmptyLines(n, ptStr))
          if (!pt.isHandType) throw new RecordFormatException(n, s"invalid piece type for hand: ${mv}")
          MoveBuilderKifHand(to, pt, t)
        case pattern(toStr, ptStr, prStr, fromStr) if fromStr != null && (prStr == "" || prStr == "成") =>
          val pt = Ptype.parseKifString(NonEmptyLines(n, ptStr))
          val from = Square.parseCsaString(NonEmptyLines(n, fromStr))
          val toOpt = if (toStr.startsWith("同")) None else Some(Square.parseKifString(NonEmptyLines(n, toStr)))
          MoveBuilderKifBoard(from, toOpt, pt, prStr == "成", t)
        case _ => throw new RecordFormatException(n, s"invalid move string: ${mv}")
      }
    }
  }
}


case class MoveBuilderKifBoard(from: Square, to: Option[Square], oldPtype: Ptype, promote: Boolean, elapsedTime: Option[Int] = None) extends MoveBuilderKif {
  override def toKifString: String =
    to.map(_.toKifString).getOrElse("同　") + oldPtype.toKifString + promote.fold("成", "") + s"(${from.toCsaString})${timeToKifString(elapsedTime)}"

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      newPtype = promote.fold(oldPiece.ptype.promoted, oldPiece.ptype)
      moveTo <- to match {
        case Some(t) => Some(t);
        case None => state.lastMoveTo
      }
      isSame = to.isEmpty
      isCheck = isCheckMove(state, Some(from), moveTo, newPtype)
      movement = getMovement(state, Some(from), moveTo, oldPiece.ptype)
      captured = state.board.get(moveTo).map(_.ptype).filter(_ != KING)
      mv <- Try(Move(state.turn, Some(from), moveTo, newPtype, promote, isSame, movement, captured, isCheck, elapsedTime, isStrict)).toOption
    } yield mv
}

case class MoveBuilderKifHand(to: Square, ptype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderKif {
  override def toKifString: String = s"${to.toKifString}${ptype.toKifString}打${timeToKifString(elapsedTime)}"

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(state.turn, None, to, ptype, promote = false, isSameSquare = false, movement, captured = None, isCheck, elapsedTime, isStrict)).toOption
    } yield mv
  }
}

