package com.mogproject.mogami.core

import com.mogproject.mogami.core.Ptype.{PAWN, LANCE, KNIGHT}

import scala.util.matching.Regex
import com.mogproject.mogami.core.io.{CsaLike, SfenLike}

/**
  * Square -- each cell on the board (and in hand)
  */
case class Square(file: Int, rank: Int) extends CsaLike with SfenLike {
  require(rank == 0 || (1 <= file && file <= 9))
  require(file == 0 || (1 <= rank && rank <= 9))

  override def toCsaString = s"${file}${rank}"

  private def rankToChar: Char = ('a' + rank - 1).toChar

  override def toSfenString = if (isHand) "*" else s"${file}${rankToChar}"

  def isHand: Boolean = file == 0 && rank == 0

  /**
    * Distance from the player's farthest rank.
    */
  def closeness(player: Player): Int = if (isHand) 0 else player.doWhenWhite(rank)(10 - _)

  def isPromotionZone(player: Player): Boolean = closeness(player) <= 3

  def isLegalZone(piece: Piece): Boolean = (piece.ptype match {
    case PAWN | LANCE => 2
    case KNIGHT => 3
    case _ => 1
  }) <= closeness(piece.owner)
}

object Square {
  def parseCsaString(s: String): Option[Square] = {
    val p: Regex = """([1-9])([1-9])""".r
    s match {
      case "00" => Some(HAND)
      case p(file, rank) => Some(Square(file.toInt, rank.toInt))
      case _ => None
    }
  }

  def parseSfenString(s: String): Option[Square] = {
    val p: Regex = """([1-9])([a-i])""".r
    s match {
      case "*" => Some(HAND)
      case p(file, rank) => Some(Square(file.toInt, rank(0) - 'a' + 1))
      case _ => None
    }
  }

  object HAND extends Square(0, 0)

}