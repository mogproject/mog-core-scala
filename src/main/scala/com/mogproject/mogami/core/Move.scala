package com.mogproject.mogami.core

import com.mogproject.mogami.core.io.{CsaLike, SfenLike}

import scala.util.matching.Regex

/**
  * Move class
  */
case class Move(from: Square,
                to: Square,
                player: Option[Player],
                newPtype: Option[Ptype],
                promote: Option[Boolean]) extends CsaLike with SfenLike {
  require(!from.isHand || newPtype.isDefined, "piece type must be defined when dropping")
  require(!from.isHand || !promote.contains(true), "promote must be undefined or false when dropping")
  require(from != to, "to must not be identical to from")
  require(!to.isHand, "to must not be in hand")

  def isCsaCompatible: Boolean = {
    player.isDefined && newPtype.isDefined
  }

  def isSfenCompatible: Boolean = {
    promote.isDefined
  }

  override def toCsaString: String = {
    require(isCsaCompatible)
    List(player.get, from, to, newPtype.get).map(_.toCsaString).mkString
  }

  override def toSfenString: String = {
    require(isSfenCompatible)
    if (from.isHand) {
      List(Piece(Player.BLACK, newPtype.get), from, to).map(_.toSfenString).mkString
    } else {
      s"${from.toSfenString}${to.toSfenString}" + (if (promote.get) "+" else "")
    }
  }
}

object Move {
  def parseCsaString(s: String): Option[Move] = {
    for {
      s <- Some(s) if s.length == 7
      pl <- Player.parseCsaString(s.substring(0, 1))
      from <- Square.parseCsaString(s.substring(1, 3))
      to <- Square.parseCsaString(s.substring(3, 5)) if to != Square.HAND && to != from
      pt <- Ptype.parseCsaString(s.substring(5))
    } yield {
      Move(from, to, Some(pl), Some(pt), None)
    }
  }

  def parseSfenString(s: String): Option[Move] = {
    val patternOnBoard: Regex = """([1-9][a-i])([1-9][a-i])([+]?)""".r
    val patternInHand: Regex = """([PLNSGBR])[*]([1-9][a-i])""".r
    s match {
      case patternOnBoard(from, to, promote) =>
        for {
          f <- Square.parseSfenString(from)
          t <- Square.parseSfenString(to) if to != from
        } yield Move(f, t, None, None, Some(promote == "+"))
      case patternInHand(ptype, to) =>
        for {
          pt <- Piece.parseSfenString(ptype)
          t <- Square.parseSfenString(to)
        } yield Move(Square.HAND, t, None, Some(pt.ptype), Some(false))
      case _ => None
    }
  }
}
