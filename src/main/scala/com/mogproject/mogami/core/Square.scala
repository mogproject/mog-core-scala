package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.WHITE

import scala.util.matching.Regex
import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

/**
  * Square -- each cell on the board (and in hand)
  */
case class Square(index: Int) extends CsaLike with SfenLike {
  require(-1 <= index && index <= 80)

  val rank: Int = (index + 9) / 9  // 0 if in hand
  val file: Int = index % 9 + 1  // 0 if in hand

  import com.mogproject.mogami.core.Direction._

  override def toCsaString = s"${file}${rank}"

  private def rankToChar: Char = ('a' + rank - 1).toChar

  override def toSfenString = if (isHand) "*" else s"${file}${rankToChar}"

  def isHand: Boolean = index < 0

  // todo: maybe deprecated
  /**
    * Distance from the player's farthest rank.
    */
  def closeness(player: Player): Int = isHand.fold(0, (player == WHITE).when[Int](10 - _)(rank))

  // todo: Deprecated
  def isPromotionZone(player: Player): Boolean = closeness(player) <= 3

  // todo: Deprecated
  def isLegalZone(piece: Piece): Boolean = (piece.ptype match {
    case PAWN | LANCE => 2
    case KNIGHT => 3
    case _ => 1
  }) <= closeness(piece.owner)

  def getBetweenBB(to: Square): BitBoard = {
    if (isHand || to.isHand) {
      BitBoard.empty
    } else {
      val f = to.file - file
      val r = to.rank - rank
      val distance = (math.abs(f), math.abs(r)) match {
        case (0, y) => y  // including the case when y == 0
        case (x, 0) => x
        case (x, y) if x == y => x
        case _ => 0
      }

      (1 until distance).foldLeft(BitBoard.empty)((b, n) => b.set(Square(file + f / distance * n, rank + r / distance * n)))
    }
  }

  // todo: Deprecated
  def getDisplacement(player: Player, to: Square): Displacement = {
    (math.abs(to.file - file), to.closeness(player) - closeness(player)) match {
      case (0, 0) => Displacement(NoRelation, 0)
      case (0, y) if y < 0 => Displacement(Forward, -y)
      case (0, y) if y > 0 => Displacement(Backward, y)
      case (x, 0) => Displacement(Side, x)
      case (x, y) if x == y => Displacement(DiagonallyBackward, x)
      case (x, y) if x == -y => Displacement(DiagonallyForward, x)
      case (1, -2) => Displacement(KnightMove, 1)
      case _ => Displacement(NoRelation, 0)
    }
  }
}

object Square extends CsaFactory[Square] with SfenFactory[Square] {

  implicit def ordering[A <: Square]: Ordering[A] = Ordering.by(s => (s.file, s.rank))

  def apply(file: Int, rank: Int): Square = {
    require(1 <= file && file <= 9 && 1 <= rank && rank <= 9)
    apply((rank - 1) * 9 + file - 1)
  }

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

  object HAND extends Square(-1)

  val BOARD: Seq[Square] = (0 until 81).map(Square.apply)

}