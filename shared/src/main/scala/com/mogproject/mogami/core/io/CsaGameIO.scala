package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.{Game, GameInfo, State}

import scala.annotation.tailrec

/**
  */
trait CsaGameIO {

}

/**
  * Reads Kif-formatted game
  */
trait CsaGameReader extends KifGameIO with CsaFactory[Game] {

  protected[io] def isStateText(t: String): Boolean = t.startsWith("P") || t == "+" || t == "-"

  protected[io] def isValidLine(ss: String): Boolean = ss.nonEmpty && !ss.startsWith("'") && !ss.startsWith("%CHUDAN")

  protected[io] def concatMoveLines(lines: List[String]): List[String] = {
    @tailrec
    def f(ss: List[String], latest: String, sofar: List[String]): List[String] = (ss, latest.isEmpty) match {
      case (x :: xs, true) => f(xs, x, sofar)
      case (x :: xs, false) if x.startsWith("T") => f(xs, "", s"${latest},${x}" :: sofar)
      case (x :: xs, false) => f(xs, x, latest :: sofar)
      case (Nil, true) => sofar.reverse
      case (Nil, false) => (latest :: sofar).reverse
    }

    f(lines, "", Nil)
  }

  @tailrec
  final protected[io] def parseMoves(chunks: List[String], pending: Option[Move], sofar: Option[Game]): Option[Game] = (chunks, pending, sofar) match {
    case (x :: Nil, None, Some(g)) if x.startsWith("%") => // ends with a special move
      MoveBuilderCsa.parseTime(x) match {
        case Some((ss, tm)) =>
          (ss match {
            case Resign.csaKeyword => Some(Resign(tm))
            case TimeUp.csaKeyword => Some(TimeUp(tm))
            case _ => None // unknown command
          }).map(sm => g.copy(finalAction = Some(sm)))
        case None => None // format error
      }
    case (IllegalMove.csaKeyword :: Nil, Some(mv), Some(g)) => // ends with an explicit illegal move
      Some(g.copy(finalAction = Some(IllegalMove(mv))))
    case (Nil, Some(_), Some(g)) => // ends with implicit illegal move
      None
    case (Nil, None, Some(g)) => sofar // ends without errors
    case (x :: xs, None, Some(g)) => MoveBuilderCsa.parseCsaString(x) match {
      case Some(bldr) => bldr.toMove(g.currentState, isStrict = false) match {
        case Some(mv) => g.makeMove(mv) match {
          case Some(gg) => parseMoves(xs, None, Some(gg))
          case None => parseMoves(xs, Some(mv), sofar)
        }
        case None => None // failed to create Move
      }
      case None => None // failed to parse Move string
    }
    case _ => None
  }

  override def parseCsaString(s: String): Option[Game] = {
    for {
      xs <- Some(s.split("[,\n]").filter(isValidLine))
      (a, ys) = xs.span(!isStateText(_))
      (b, c) = ys.span(isStateText)
      gi <- GameInfo.parseCsaString(a)
      st <- State.parseCsaString(b)
      chunks = concatMoveLines(c.toList)
      game <- parseMoves(chunks, None, Some(Game(st, Vector.empty, gi)))
    } yield game
  }

}