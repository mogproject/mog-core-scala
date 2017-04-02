package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core._
import com.mogproject.mogami.util.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class SfenStateReaderSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestSfenStateReader extends SfenStateReader

  val dataForTest = Seq(
    State.HIRATE,
    State.empty,
    State(WHITE, Map(
      P11 -> WL, P21 -> WN, P22 -> WS, P32 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P82 -> WR,
      P14 -> WP, P23 -> WP, P34 -> WP, P43 -> WP, P53 -> WP, P63 -> BPB, P73 -> WP, P83 -> WP, P93 -> WP,
      P16 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P75 -> BP, P87 -> BP, P97 -> BP,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P58 -> BG, P78 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS ++ Map(BP -> 1, BB -> 1, WP -> 1, WR -> 1).mapKeys(Hand.apply)),
    State(BLACK, Map(
      P11 -> WPL, P21 -> WPN, P31 -> WPS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WPS, P81 -> WPN, P91 -> WPL,
      P22 -> WPB, P82 -> WPR,
      P13 -> WPP, P23 -> WPP, P33 -> WPP, P43 -> WPP, P53 -> WPP, P63 -> WPP, P73 -> WPP, P83 -> WPP, P93 -> WPP,
      P17 -> BPP, P27 -> BPP, P37 -> BPP, P47 -> BPP, P57 -> BPP, P67 -> BPP, P77 -> BPP, P87 -> BPP, P97 -> BPP,
      P28 -> BPR, P88 -> BPB,
      P19 -> BPL, P29 -> BPN, P39 -> BPS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BPS, P89 -> BPN, P99 -> BPL
    ), State.EMPTY_HANDS),
    State(BLACK, Map(P51 -> WK),
      State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2, BR -> 2).mapKeys(Hand.apply)),
    State(WHITE, Map(P59 -> BK),
      State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2, WR -> 2).mapKeys(Hand.apply))
  )

  "SfenStateReader#parseSfenString" must "parse states" in {
    TestSfenStateReader.parseSfenString("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b -") mustBe dataForTest(0)
    TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9 b -") mustBe dataForTest(1)
    TestSfenStateReader.parseSfenString("lnsgk2nl/1r4gs1/ppp+Bpp1p1/6p1p/2P6/8P/PP2PPPP1/2S1G4/LN2KGSNL w BPrp") mustBe dataForTest(2)
    TestSfenStateReader.parseSfenString("+l+n+sgkg+s+n+l/1+r5+b1/+p+p+p+p+p+p+p+p+p/9/9/9/+P+P+P+P+P+P+P+P+P/1+B5+R1/+L+N+SGKG+S+N+L b -") mustBe dataForTest(3)
    TestSfenStateReader.parseSfenString("4k4/9/9/9/9/9/9/9/9 b 2R2B4G4S4N4L18P") mustBe dataForTest(4)
    TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/4K4 w 2r2b4g4s4n4l18p") mustBe dataForTest(5)
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9/9"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9/9 b - -"))

    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("8/9/9/9/9/9/9/9/9 b -"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("4k5/9/9/9/9/9/9/9/9 b -"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/4k5 b -"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9/9 b -"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/8+/9/9 B -"))

    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9 B -"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9 b +"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9 b 3"))
    assertThrows[RecordFormatException](TestSfenStateReader.parseSfenString("9/9/9/9/9/9/9/9/9 b x"))
  }

}
