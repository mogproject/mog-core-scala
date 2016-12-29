package com.mogproject.mogami.core

import com.mogproject.mogami._
import org.scalacheck.Gen

/**
  * Square generator for scalacheck
  */
object SquareGen {
  val boards: Seq[Square] = for {f <- 1 to 9; r <- 1 to 9} yield Square(f, r)

  val squares: Gen[Square] = Gen.oneOf(HAND +: boards)

  val squaresOnBoard: Gen[Square] = Gen.oneOf(boards)

  def squaresOnBoardExcept(except: Seq[Square]): Gen[Square] = Gen.oneOf((boards.toSet -- except).toSeq)

}

/**
  * Constant values
  */
object SquareConstant {
  val P11 = Square(1, 1)
  val P12 = Square(1, 2)
  val P13 = Square(1, 3)
  val P14 = Square(1, 4)
  val P15 = Square(1, 5)
  val P16 = Square(1, 6)
  val P17 = Square(1, 7)
  val P18 = Square(1, 8)
  val P19 = Square(1, 9)
  val P21 = Square(2, 1)
  val P22 = Square(2, 2)
  val P23 = Square(2, 3)
  val P24 = Square(2, 4)
  val P25 = Square(2, 5)
  val P26 = Square(2, 6)
  val P27 = Square(2, 7)
  val P28 = Square(2, 8)
  val P29 = Square(2, 9)
  val P31 = Square(3, 1)
  val P32 = Square(3, 2)
  val P33 = Square(3, 3)
  val P34 = Square(3, 4)
  val P35 = Square(3, 5)
  val P36 = Square(3, 6)
  val P37 = Square(3, 7)
  val P38 = Square(3, 8)
  val P39 = Square(3, 9)
  val P41 = Square(4, 1)
  val P42 = Square(4, 2)
  val P43 = Square(4, 3)
  val P44 = Square(4, 4)
  val P45 = Square(4, 5)
  val P46 = Square(4, 6)
  val P47 = Square(4, 7)
  val P48 = Square(4, 8)
  val P49 = Square(4, 9)
  val P51 = Square(5, 1)
  val P52 = Square(5, 2)
  val P53 = Square(5, 3)
  val P54 = Square(5, 4)
  val P55 = Square(5, 5)
  val P56 = Square(5, 6)
  val P57 = Square(5, 7)
  val P58 = Square(5, 8)
  val P59 = Square(5, 9)
  val P61 = Square(6, 1)
  val P62 = Square(6, 2)
  val P63 = Square(6, 3)
  val P64 = Square(6, 4)
  val P65 = Square(6, 5)
  val P66 = Square(6, 6)
  val P67 = Square(6, 7)
  val P68 = Square(6, 8)
  val P69 = Square(6, 9)
  val P71 = Square(7, 1)
  val P72 = Square(7, 2)
  val P73 = Square(7, 3)
  val P74 = Square(7, 4)
  val P75 = Square(7, 5)
  val P76 = Square(7, 6)
  val P77 = Square(7, 7)
  val P78 = Square(7, 8)
  val P79 = Square(7, 9)
  val P81 = Square(8, 1)
  val P82 = Square(8, 2)
  val P83 = Square(8, 3)
  val P84 = Square(8, 4)
  val P85 = Square(8, 5)
  val P86 = Square(8, 6)
  val P87 = Square(8, 7)
  val P88 = Square(8, 8)
  val P89 = Square(8, 9)
  val P91 = Square(9, 1)
  val P92 = Square(9, 2)
  val P93 = Square(9, 3)
  val P94 = Square(9, 4)
  val P95 = Square(9, 5)
  val P96 = Square(9, 6)
  val P97 = Square(9, 7)
  val P98 = Square(9, 8)
  val P99 = Square(9, 9)

}