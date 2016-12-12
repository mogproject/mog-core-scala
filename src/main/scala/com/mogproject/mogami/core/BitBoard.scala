package com.mogproject.mogami.core

import com.mogproject.mogami.util.BitOperation
import com.mogproject.mogami.core.Player.{BLACK, WHITE}


case class BitBoard(lo: Long = 0L, hi: Long = 0L) {
  require(0L <= lo && lo <= BitBoard.MASK54)
  require(0L <= hi && hi <= BitBoard.MASK27)

  def get(pos: Int): Boolean = getProcess(pos)((a, b) => (a & b) != 0L)

  def get(pos: Square): Boolean = get(pos.index)

  def set(pos: Int): BitBoard = setProcess(pos)(_ | _)

  def set(pos: Square): BitBoard = set(pos.index)

  def reset(pos: Int): BitBoard = setProcess(pos)((a, b) => a & (BitBoard.MASK54 ^ b))

  def reset(pos: Square): BitBoard = reset(pos.index)

  def isEmpty: Boolean = (lo | hi) == 0L

  def count: Int = BitOperation.pop(lo) + BitOperation.pop(hi)

  def unary_~ : BitBoard = this ^ BitBoard.full

  def &(that: BitBoard): BitBoard = operate(_ & _)(that)

  def |(that: BitBoard): BitBoard = operate(_ | _)(that)

  def ^(that: BitBoard): BitBoard = operate(_ ^ _)(that)

  override def toString: String = {
    (0 until 9).map { i =>
      (0 until 9).map { j =>
        if (get(i * 9 + 8 - j)) "*" else "-"
      }.mkString
    }.mkString("\n")
  }

  def toSet: Set[Square] = {
    // TODO: need optimization using JNA
    Square.BOARD.filter(get).toSet
  }

  private[this] def operate(f: (Long, Long) => Long)(that: BitBoard) = BitBoard(f(lo, that.lo), f(hi, that.hi))

  private[this] def setProcess(pos: Int)(f: (Long, Long) => Long) = {
    require(0 <= pos && pos < 81)
    if (pos < 54) {
      BitBoard(f(lo, 1L << pos), hi)
    } else {
      BitBoard(lo, f(hi, 1L << (pos - 54)))
    }
  }

  private[this] def getProcess(pos: Int)(f: (Long, Long) => Boolean) = {
    require(0 <= pos && pos < 81)
    if (pos < 54) {
      f(lo, 1L << pos)
    } else {
      f(hi, 1L << (pos - 54))
    }
  }

  /**
    * Shift all bits to right.
    *
    * @param n shift width
    *
    *          e.g. n=2
    *          *********       --*******
    *          -*-----*-       ---*-----
    *          *--***-**       --*--***-
    *          ------*--       --------*
    *          -*-------   =>  ---*-----
    *          --*----*-       ----*----
    *          *******-*       --*******
    *          -------*-       ---------
    *          *********       --*******
    */
  def shiftRight(n: Int): BitBoard = n match {
    case x if x < 0 => shiftLeft(-n)
    case x if x == 0 => this
    case x if 9 <= x => BitBoard.empty
    case _ => {
      val mask = (0x1ffL >> n << n) * 0x201008040201L
      BitBoard((lo & mask) >> n, (hi & mask) >> n)
    }
  }

  /**
    * Shift all bits to left.
    *
    * @param n shift width
    *
    *          e.g. n=2
    *          *********       *******--
    *          -*-----*-       -----*---
    *          *--***-**       -***-**--
    *          ------*--       ----*----
    *          -*-------   =>  ---------
    *          --*----*-       *----*---
    *          *******-*       *****-*--
    *          -------*-       -----*---
    *          *********       *******--
    */
  def shiftLeft(n: Int): BitBoard = n match {
    case x if x < 0 => shiftRight(-n)
    case x if x == 0 => this
    case x if 9 <= x => BitBoard.empty
    case _ => {
      val mask = (((0x1ffL << n) & 0x1ffL) >> n) * 0x201008040201L
      BitBoard(((lo & mask) << n) & BitBoard.MASK54, ((hi & mask) << n) & BitBoard.MASK27)
    }
  }

  /**
    * Shift all bits to up.
    *
    * @param n shift width
    *
    *          e.g. n=2
    *          *********       *--***-**
    *          -*-----*-       ------*--
    *          *--***-**       -*-------
    *          ------*--       --*----*-
    *          -*-------   =>  *******-*
    *          --*----*-       -------*-
    *          *******-*       *********
    *          -------*-       ---------
    *          *********       ---------
    */
  def shiftUp(n: Int): BitBoard = n match {
    case x if x < 0 => shiftDown(-n)
    case x if x == 0 => this
    case x if 9 <= x => BitBoard.empty
    case _ => {
      val x = 9 * n
      val y = math.min(x, 63)
      val carry = if (n <= 6) (hi << (54 - x)) & BitBoard.MASK54 else hi >> (x - 54)
      BitBoard((lo >> y) | carry, hi >> y)
    }
  }

  /**
    * Shift all bits to down.
    *
    * @param n shift width
    *
    *          e.g. n=2
    *          *********       ---------
    *          -*-----*-       ---------
    *          *--***-**       *********
    *          ------*--       -*-----*-
    *          -*-------   =>  *--***-**
    *          --*----*-       ------*--
    *          *******-*       -*-------
    *          -------*-       --*----*-
    *          *********       *******-*
    */
  def shiftDown(n: Int): BitBoard = n match {
    case x if x < 0 => shiftUp(-x)
    case x if x == 0 => this
    case x if 9 <= x => BitBoard.empty
    case _ => {
      val x = 9 * n
      val y = math.min(x, 63)
      val carry = if (n <= 6) lo >>> (54 - x) else lo << (x - 54)
      BitBoard((lo << y) & BitBoard.MASK54, (carry | (hi << y)) & BitBoard.MASK27)
    }
  }

  /**
    * Flip a bitboard vertically about the centre ranks.
    * Rank 1 is mapped to rank 9 and vice versa.
    *
    * e.g.
    * *********       *********
    * -*-----*-       -------*-
    * *--***-**       *******-*
    * ------*--       --*----*-
    * -*-------   =>  -*-------
    * --*----*-       ------*--
    * *******-*       *--***-**
    * -------*-       -*-----*-
    * *********       *********
    */
  def flipVertical: BitBoard = {
    val x = ((lo << 18) & 0x3fe00007fc0000L) + (lo & 0x1FF00003FE00L) + ((lo >>> 18) & 0xff80001ffL)
    // rank 321654
    val y = (hi >>> 18) + (hi & 0x3fe00L) + (hi << 18) // rank 98789
    BitBoard((y & BitBoard.MASK27) + (x & 0x3ffffff8000000L), x & BitBoard.MASK27)
  }

  /**
    * Mirror a bitboard horizontally about the center files.
    * File 1 is mapped to file 9 and vice versa.
    *
    * e.g.
    * *********       *********
    * -*-----*-       -*-----*-
    * *--***-**       **-***--*
    * ------*--       --*------
    * -*-------   =>  -------*-
    * --*----*-       -*----*--
    * *******-*       *-*******
    * -------*-       -*-------
    * *********       *********
    */
  def mirrorHorizontal: BitBoard = {
    val x = ((lo & 0x2954aa552a954aL) >>> 1) + ((lo & 0x14aa552a954aa5L) << 1)
    val y = ((x & 0x3198cc6633198cL) >>> 2) + ((x & 0xc6633198cc663L) << 2)
    val z = ((y & 0x3c1e0f0783c1e0L) >>> 5) + (lo & 0x2010080402010L) + ((y & 0x1e0f0783c1e0fL) << 5)
    val a = ((hi & 0x52a954aL) >>> 1) + ((hi & 0x2954aa5L) << 1)
    val b = ((a & 0x633198cL) >>> 2) + ((a & 0x18cc663L) << 2)
    val c = ((b & 0x783c1e0L) >>> 5) + (hi & 0x402010L) + ((b & 0x3c1e0fL) << 5)
    BitBoard(z, c)
  }

  /**
    * Spread each bit to all file-direction.
    *
    * e.g.
    * ---------      ****-*--*
    * ---------      ****-*--*
    * ---------      ****-*--*
    * ---------      ****-*--*
    * ---*-----  =>  ****-*--*
    * *-*--*--*      ****-*--*
    * -*-------      ****-*--*
    * ---------      ****-*--*
    * ---------      ****-*--*
    */
  def spreadAllFile: BitBoard = {
    var x = lo | hi
    x |= x >> 27
    x |= x * 0x00040200L
    x = (x >> 18) & 0x000001ffL
    x *= 0x0000201008040201L
    BitBoard(x & BitBoard.MASK54, x & BitBoard.MASK27)
  }

}

object BitBoard {
  private val MASK54 = (1L << 54) - 1L
  private val MASK27 = (1L << 27) - 1L
  val empty = BitBoard(0L, 0L)
  val full = BitBoard(MASK54, MASK27)
  val EDGE = BitBoard(
    """
      |*********
      |*-------*
      |*-------*
      |*-------*
      |*-------*
      |*-------*
      |*-------*
      |*-------*
      |*********
    """.stripMargin)
  val INNER = ~EDGE

  def ident(pos: Int): BitBoard = {
    require(0 <= pos && pos < 81)
    if (pos < 54) {
      BitBoard(1L << pos, 0L)
    } else {
      BitBoard(0L, 1L << (pos - 54))
    }
  }

  def ident(p: Square): BitBoard = ident(p.index)

  /**
    * Make bitboard sequence from long-width string lines
    *
    * e.g.
    * BitBoard.seq("""
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * ---------  *********
    * """) == Seq(BitBoard("""
    * ---------
    * ---------
    * ---------
    * ---------
    * ---------
    * ---------
    * ---------
    * ---------
    * ---------
    * """), BitBoard("""
    * *********
    * *********
    * *********
    * *********
    * *********
    * *********
    * *********
    * *********
    * *********
    * """))
    *
    * @param s string
    * @return
    */
  def seq(s: String): Seq[BitBoard] = {
    s.split('\n').withFilter(!_.isEmpty).map(_.split("[ ]+")).transpose map (a => BitBoard(a.mkString))
  }

  private[this] val attackingThirdImpl: Map[Player, BitBoard] = (Seq(BLACK, WHITE) zip seq(
    """
      |********* ---------
      |********* ---------
      |********* ---------
      |--------- ---------
      |--------- ---------
      |--------- ---------
      |--------- *********
      |--------- *********
      |--------- *********
    """.stripMargin)).toMap

  /**
    * Get the attacking third positions
    *
    * @param t turn to calculate
    * @return
    */
  def attackingThird(t: Player): BitBoard = attackingThirdImpl(t)

  def apply(s: String): BitBoard = {
    val xs = s.toList.flatMap { case '-' => List(0L); case '*' => List(1L); case _ => Nil }

    def f(xs: List[Long]): Long = xs.zipWithIndex.foldLeft(0L) {
      case (x, (y, i)) => x | (y << (i / 9 * 9 + 8 - i % 9))
    }

    BitBoard(f(xs.take(54)), f(xs.drop(54)))
  }


}