package com.mogproject.mogami.core

import com.mogproject.mogami.core.Ptype._

import scala.util.matching.Regex
import com.mogproject.mogami.core.io.{CsaFactory, CsaLike, SfenFactory, SfenLike}

/**
  * Square -- each cell on the board (and in hand)
  */
case class Square(file: Int, rank: Int) extends CsaLike with SfenLike {
  require(rank == 0 || (1 <= file && file <= 9))
  require(file == 0 || (1 <= rank && rank <= 9))

  import com.mogproject.mogami.core.Direction._

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

  def getInnerSquares(to: Square): Seq[Square] = {
    if (isHand || to.isHand) {
      Seq.empty
    } else {
      val f = to.file - file
      val r = to.rank - rank
      val distance = (math.abs(f), math.abs(r)) match {
        case (0, 0) => None
        case (0, y) => Some(y)
        case (x, 0) => Some(x)
        case (x, y) if x == y => Some(x)
        case _ => None
      }

      (for {
        d <- distance
      } yield (1 until d).map(n => Square(file + f / d * n, rank + r / d * n))).getOrElse(Seq.empty)
    }
  }

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