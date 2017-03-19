package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.io.{KifFactory, KifLike}
import com.mogproject.mogami.util.Implicits._

import scala.util.matching.Regex
import scala.util.{Success, Try}

/**
  *
  */
sealed trait MoveBuilderKif extends MoveBuilder with KifLike


object MoveBuilderKif extends KifFactory[MoveBuilderKif] {
  private[this] val patternTime: Regex = """([^ ]+)(?:[ ]+[(][ ]*(\d+):[ ]*(\d+)[/](?:[ ]*(\d+):[ ]*(\d+):[ ]*(\d+))?[)])?""".r
  private[this] val pattern: Regex = """(..)([成]?.)([成打]?)(?:[(]([1-9]{2})[)])?""".r

  private[this] def parseTime(s: String): Option[(String, Option[Int])] = s match {
    case patternTime(mv, null, null, null, null, null) => Some(mv, None)
    case patternTime(mv, mm, ss, _, _, _) => (Try(mm.toInt), Try(ss.toInt)) match {
      case (Success(mx), Success(sx)) if mx <= Int.MaxValue / 60 && sx < 60 => Some(mv, Some(mx * 60 + sx))
      case _ => None
    }
    case _ => None
  }

  override def parseKifString(s: String): Option[MoveBuilderKif] = parseTime(s) match {
    case Some((pattern(toStr, ptStr, "打", null), t)) =>
      for {
        to <- Square.parseKifString(toStr)
        pt <- Ptype.parseKifString(ptStr)
        if !pt.isPromoted
      } yield {
        MoveBuilderKifHand(to, pt, t)
      }
    case Some((pattern(toStr, ptStr, prStr, fromStr), t)) if prStr == "" || prStr == "成" =>
      for {
        pt <- Ptype.parseKifString(ptStr)
        from <- Square.parseCsaString(fromStr)
        toOpt = Square.parseKifString(toStr)
        if toOpt.isDefined || toStr == "同　"
      } yield {
        MoveBuilderKifBoard(from, toOpt, pt, prStr == "成", t)
      }
    case _ => None
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
      captured = state.board.get(moveTo).map(_.ptype)
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

