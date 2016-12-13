package com.mogproject.mogami.core

import com.mogproject.mogami.util.BitOperation
import com.mogproject.mogami.util.BooleanOps.Implicits._


class BitBoard(val lo: Long, val hi: Long) {
  require(0L <= lo && lo <= BitBoard.MASK54)
  require(0L <= hi && hi <= BitBoard.MASK27)

  override def equals(other: Any): Boolean = other match {
    case that: BitBoard => this.lo == that.lo && this.hi == that.hi
    case _ => false
  }

  override def hashCode(): Int = 41 * lo.hashCode() + hi.hashCode()

  def unary_~ : BitBoard = this ^ BitBoard.full

  def &(that: BitBoard): BitBoard = operate(_ & _)(that)

  def |(that: BitBoard): BitBoard = operate(_ | _)(that)

  def ^(that: BitBoard): BitBoard = operate(_ ^ _)(that)

  override def toString: String =
    (0 until 9).map { i => (8 to 0 by -1).map { j => get(i * 9 + j).fold("*", "-") }.mkString }.mkString("\n")

  def toOctalString: String =
    (0 until 9).map { i => (2 to 0 by -1).map { j => ((i < 6).fold(lo, hi) >> (i % 6 * 9 + j * 3)) & 7 }.mkString }.mkString(".")

  def get(index: Int): Boolean = getProcess(index)((a, b) => (a & b) != 0L)

  def get(sq: Square): Boolean = get(sq.index)

  def set(index: Int): BitBoard = setProcess(index)(_ | _)

  def set(sq: Square): BitBoard = set(sq.index)

  def reset(index: Int): BitBoard = setProcess(index)((a, b) => a & (BitBoard.MASK54 ^ b))

  def reset(sq: Square): BitBoard = reset(sq.index)

  def isEmpty: Boolean = (lo | hi) == 0L

  def isDefined: Boolean = !isEmpty

  def count: Int = BitOperation.pop(lo) + BitOperation.pop(hi)

  def toList: List[Square] = Square.BOARD.filter(get).toList

  def toSet: Set[Square] = toList.toSet

  private[this] def operate(f: (Long, Long) => Long)(that: BitBoard) = BitBoard(f(lo, that.lo), f(hi, that.hi))

  private[this] def setProcess(index: Int)(f: (Long, Long) => Long) = {
    require(0 <= index && index < 81)
    if (index < 54) {
      BitBoard(f(lo, 1L << index), hi)
    } else {
      BitBoard(lo, f(hi, 1L << (index - 54)))
    }
  }

  private[this] def getProcess(index: Int)(f: (Long, Long) => Boolean) = {
    require(0 <= index && index < 81)
    if (index < 54) {
      f(lo, 1L << index)
    } else {
      f(hi, 1L << (index - 54))
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
    case _ =>
      val mask = (0x1ffL >> n << n) * 0x201008040201L
      BitBoard((lo & mask) >> n, (hi & mask) >> n)
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
    case _ =>
      val mask = (((0x1ffL << n) & 0x1ffL) >> n) * 0x201008040201L
      BitBoard(((lo & mask) << n) & BitBoard.MASK54, ((hi & mask) << n) & BitBoard.MASK27)
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
    case _ =>
      val x = 9 * n
      val y = math.min(x, 63)
      val carry = if (n <= 6) (hi << (54 - x)) & BitBoard.MASK54 else hi >> (x - 54)
      BitBoard((lo >> y) | carry, hi >> y)
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
    case _ =>
      val x = 9 * n
      val y = math.min(x, 63)
      val carry = if (n <= 6) lo >>> (54 - x) else lo << (x - 54)
      BitBoard((lo << y) & BitBoard.MASK54, (carry | (hi << y)) & BitBoard.MASK27)
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
  def flipHorizontal: BitBoard = {
    val x = ((lo & 0x2954aa552a954aL) >>> 1) + ((lo & 0x14aa552a954aa5L) << 1)
    val y = ((x & 0x3198cc6633198cL) >>> 2) + ((x & 0xc6633198cc663L) << 2)
    val z = ((y & 0x3c1e0f0783c1e0L) >>> 5) + (lo & 0x2010080402010L) + ((y & 0x1e0f0783c1e0fL) << 5)
    val a = ((hi & 0x52a954aL) >>> 1) + ((hi & 0x2954aa5L) << 1)
    val b = ((a & 0x633198cL) >>> 2) + ((a & 0x18cc663L) << 2)
    val c = ((b & 0x783c1e0L) >>> 5) + (hi & 0x402010L) + ((b & 0x3c1e0fL) << 5)
    BitBoard(z, c)
  }

  /**
    * Filp the bitboard vertically when the player is white.
    *
    * @param player Player instance
    */
  def flipByPlayer(player: Player): BitBoard = player.isWhite.when { bb: BitBoard => bb.flipVertical }(this)

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
  def spreadFiles: BitBoard = {
    var x = lo | hi
    x |= x >> 27
    x |= x * 0x00040200L
    x = (x >> 18) & 0x000001ffL
    x *= 0x0000201008040201L
    BitBoard(x, x)
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
  val INNER: BitBoard = ~EDGE

  def ident(index: Int): BitBoard = {
    require(0 <= index && index < 81)
    if (index < 54) {
      BitBoard(1L << index, 0L)
    } else {
      BitBoard(0L, 1L << (index - 54))
    }
  }

  def ident(sq: Square): BitBoard = ident(sq.index)

  def promotion(player: Player): BitBoard = BitBoard(0x7ffffffL, 0L).flipByPlayer(player)

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

  /**
    * constructor
    *
    * @param hi rank1-6
    * @param lo rank7-9
    * @return bitboard
    */
  def apply(hi: Long = 0L, lo: Long = 0L): BitBoard = new BitBoard(hi & BitBoard.MASK54, lo & BitBoard.MASK27)

  /** *
    * String-based constructor
    *
    * @param s string
    *          1) binary format: use `-` for 0-bit and `*` for 1-bit
    *          (e.g. "*********\n*********\n*********\n---------\n---------\n---------\n---------\n---------\n---------")
    *          2) octal format: octal integers (e.g. "777.777.777.000.000.000.000.000.000")
    * @return bitboard
    */
  def apply(s: String): BitBoard = {
    val isBinaryFormat = s.exists(c => c == '-' || c == '*')

    if (isBinaryFormat) {
      val xs = s.toList.flatMap { case '-' => List(0L); case '*' => List(1L); case _ => Nil }

      def f(xs: List[Long]): Long = xs.zipWithIndex.foldLeft(0L) {
        case (x, (y, i)) => x | (y << (i / 9 * 9 + 8 - i % 9))
      }

      BitBoard(f(xs.take(54)), f(xs.drop(54)))
    } else {
      val xs = s.toList.flatMap { case n if '0' <= n && n <= '7' => List((n - '0').toLong); case _ => Nil }

      def f(xs: List[Long]): Long = xs.zipWithIndex.foldLeft(0L) {
        case (x, (y, i)) => x | (y << ((i + (1 - i % 3) * 2) * 3))
      }

      BitBoard(f(xs.take(18)), f(xs.drop(18)))
    }
  }

  /**
    * Rank constants
    */
  final val rank1 = BitBoard("777.000.000.000.000.000.000.000.000")
  final val rank2 = BitBoard("000.777.000.000.000.000.000.000.000")
  final val rank3 = BitBoard("000.000.777.000.000.000.000.000.000")
  final val rank4 = BitBoard("000.000.000.777.000.000.000.000.000")
  final val rank5 = BitBoard("000.000.000.000.777.000.000.000.000")
  final val rank6 = BitBoard("000.000.000.000.000.777.000.000.000")
  final val rank7 = BitBoard("000.000.000.000.000.000.777.000.000")
  final val rank8 = BitBoard("000.000.000.000.000.000.000.777.000")
  final val rank9 = BitBoard("000.000.000.000.000.000.000.000.777")

  /**
    * File constants
    */
  final val file1 = BitBoard("001.001.001.001.001.001.001.001.001")
  final val file2 = BitBoard("002.002.002.002.002.002.002.002.002")
  final val file3 = BitBoard("004.004.004.004.004.004.004.004.004")
  final val file4 = BitBoard("010.010.010.010.010.010.010.010.010")
  final val file5 = BitBoard("020.020.020.020.020.020.020.020.020")
  final val file6 = BitBoard("040.040.040.040.040.040.040.040.040")
  final val file7 = BitBoard("100.100.100.100.100.100.100.100.100")
  final val file8 = BitBoard("200.200.200.200.200.200.200.200.200")
  final val file9 = BitBoard("400.400.400.400.400.400.400.400.400")

}