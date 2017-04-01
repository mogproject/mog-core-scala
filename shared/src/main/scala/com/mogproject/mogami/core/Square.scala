package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.WHITE

import scala.util.matching.Regex
import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

/**
  * Square -- each cell on the board
  */
case class Square(index: Int) extends CsaLike with SfenLike with KifLike {
  require(0 <= index && index < 81)

  import com.mogproject.mogami.core.Direction._

  val rank: Int = (index + 9) / 9
  val file: Int = index % 9 + 1

  def unary_! : Square = Square(10 - file, 10 - rank)

  override def toCsaString = s"${file}${rank}"

  private def rankToChar: Char = ('a' + rank - 1).toChar

  override def toSfenString = s"${file}${rankToChar}"

  override def toKifString: String = s"${"１２３４５６７８９".charAt(file - 1)}${"一二三四五六七八九".charAt(rank - 1)}"

  /**
    * Distance from the player's farthest rank.
    */
  def closeness(player: Player): Int = (player == WHITE).when[Int](10 - _)(rank)

  def isPromotionZone(player: Player): Boolean = closeness(player) <= 3

  def isLegalZone(piece: Piece): Boolean = (piece.ptype match {
    case PAWN | LANCE => 2
    case KNIGHT => 3
    case _ => 1
  }) <= closeness(piece.owner)

  def getBetweenBB(to: Square): BitBoard = {
    val f = to.file - file
    val r = to.rank - rank
    val distance = (math.abs(f), math.abs(r)) match {
      case (0, y) => y // including the case when y == 0
      case (x, 0) => x
      case (x, y) if x == y => x
      case _ => 0
    }

    (1 until distance).foldLeft(BitBoard.empty)((b, n) => b.set(Square(file + f / distance * n, rank + r / distance * n)))
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

object Square extends CsaFactory[Square] with SfenFactory[Square] with KifFactory[Square] {

  implicit def ordering[A <: Square]: Ordering[A] = Ordering.by(_.index)

  def apply(file: Int, rank: Int): Square = {
    require(1 <= file && file <= 9 && 1 <= rank && rank <= 9)
    apply((rank - 1) * 9 + file - 1)
  }

  override def parseCsaString(nel: NonEmptyLines): Square = {
    if (nel.lines.length >= 2) {
      val (x, n) = nel.lines(1)
      throw new RecordFormatException(n, s"too long square expression: ${x}")
    } else {
      val p: Regex = """([1-9])([1-9])""".r
      nel.lines.head match {
        case (p(file, rank), _) => Square(file.toInt, rank.toInt)
        case (x, n) => throw new RecordFormatException(n, s"invalid square: ${x}")
      }
    }
  }

  override def parseSfenString(s: String): Option[Square] = {
    val p: Regex = """([1-9])([a-i])""".r
    s match {
      case p(file, rank) => Some(Square(file.toInt, rank(0) - 'a' + 1))
      case _ => None
    }
  }

  override def parseKifString(nel: NonEmptyLines): Square = {
    if (nel.lines.length >= 2) {
      val (x, n) = nel.lines(1)
      throw new RecordFormatException(n, s"too long square expression: ${x}")
    } else {
      val (x, n) = nel.lines.head
      if (x.length != 2) {
        throw new RecordFormatException(n, s"invalid square length: ${x}")
      } else {
        val f = "１２３４５６７８９".indexOf(x(0))
        val r = "一二三四五六七八九".indexOf(x(1))
        if (f >= 0 && r >= 0) {
          Square(f + 1, r + 1)
        } else {
          throw new RecordFormatException(n, s"invalid square format: ${x}")
        }
      }
    }
  }

  val all: Seq[Square] = (0 until 81).map(Square.apply)

}