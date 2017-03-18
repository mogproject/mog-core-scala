package com.mogproject.mogami.core.io

import com.mogproject.mogami._
import com.mogproject.mogami.util.Implicits._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Reads Kif-formatted state
  */
trait KifStateReader extends KifFactory[State] {

  private[this] def parseNumber(s: String, default: Int): Option[Int] = {
    def f(c: Char): Option[Int] = "一二三四五六七八九".indexOf(c) match {
      case n if n >= 0 => Some(n + 1)
      case _ => None
    }

    s.toList match {
      case Nil => Some(default)
      case '十' :: Nil => Some(10)
      case '十' :: c :: Nil => f(c).map(_ + 10)
      case c :: Nil => f(c)
      case _ => None
    }
  }
  protected[io] def parseHandExpression(s: String, player: Player): Option[HandType] = {
    def f(h: String): Option[(Hand, Int)] = {
      (Ptype.parseKifString(h.take(1)), parseNumber(h.drop(1), 1)) match {
        case (Some(pt), Some(n)) => Some(Hand(player, pt) -> n)
        case _ => None
      }
    }

    if (s == "なし") {
      Some(Map.empty)
    } else {
      s.split('　').map(f).foldLeft[Option[HandType]](Some(Map.empty)) {
        case (Some(a), Some(b)) => Some(a + b)
        case _ => None
      }
    }
  }

  protected[io] def parseBoardExpression(lines: List[String]): Option[BoardType] = {
    @tailrec
    def f(sofar: Option[BoardType], ls: List[String], rank: Int): Option[BoardType] = (sofar, ls, rank) match {
      case (Some(_), x :: xs, _) if x.length == 3 + 2 * 9 =>
        f(parseOneLine(sofar, x.slice(1, 1 + 2 * 9).grouped(2).toList, 9, rank), xs, rank + 1)
      case (_, Nil, 10) => sofar
      case _ =>
        // println(s"Invalid board expression/: lines=${ls}, rank=${rank}")
        None
    }

    def parseOneLine(sofar: Option[BoardType], ls: List[String], file: Int, rank: Int): Option[BoardType] = (sofar, ls, file) match {
      case (Some(_), x :: xs, _) if x == " ・" => // no piece
        parseOneLine(sofar, xs, file - 1, rank)
      case (Some(b), x :: xs, _) => // add one piece
        val pos = Square(file, rank)
        Piece.parseKifString(x) match {
          case Some(p) => parseOneLine(Some(b.updated(pos, p)), xs, file - 1, rank)
          case _ => None // invalid piece string
        }
      case (_, Nil, 0) => sofar
      case _ => None
    }

    f(Some(Map.empty), lines.slice(2, 11), 1)
  }

  override def parseKifString(s: String): Option[State] = {
    @tailrec
    def f(ss: List[String], sofar: Option[(BoardType, HandType)]): Option[State] = {
      (ss, sofar) match {
        case (x :: xs, Some((b, h))) if x.slice(1, 6) == "手の持駒：" =>
          // todo: more strict check?
          parseHandExpression(x.drop(6), x.headOption.contains('先').fold(BLACK, WHITE)) match {
            case Some(hh) => f(xs, Some((b, h ++ hh)))
            case None => None
          }
        case (xs, Some((b, h))) if xs.length <= 1 => // Turn to move should be written in the last line
          Player.parseKifString(xs.mkString("")) flatMap { t =>
            Try(State(t, b, h)).toOption }
        case (x :: _, Some((_, h))) if x == "  ９ ８ ７ ６ ５ ４ ３ ２ １" =>
          parseBoardExpression(ss.take(12)) match {
            case Some(b) => f(ss.drop(12), Some(b, h))
            case None => None
          }
        case _ => None
      }
    }

    f(s.split('\n').toList, Some((Map.empty, State.EMPTY_HANDS)))
  }

}
