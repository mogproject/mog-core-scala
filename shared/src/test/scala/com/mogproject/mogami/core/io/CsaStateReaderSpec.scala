package com.mogproject.mogami.core.io

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.State.HIRATE
import com.mogproject.mogami.util.Implicits._

class CsaStateReaderSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestCsaStateReader extends CsaStateReader

  val initResult: Option[(BoardType, HandType, Map[Ptype, Int])] = Some((Map.empty, State.EMPTY_HANDS, State.capacity))
  val zeroCap: Map[Ptype, Int] = State.capacity.mapValues(_ => 0)
  val hirateResult: Option[(BoardType, HandType, Map[Ptype, Int])] = Some((HIRATE.board, HIRATE.hand, zeroCap))

  val dataForTest = Seq(
    State.HIRATE,
    State(BLACK, Map(), State.EMPTY_HANDS),
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

  val csaForTest = Seq(
    "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY\n" +
      "P2 * -HI *  *  *  *  * -KA * \n" +
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU\n" +
      "P8 * +KA *  *  *  *  * +HI * \n" +
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY\n" +
      "P+\n" +
      "P-\n" +
      "+",
    "P1 *  *  *  *  *  *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  *  *  *  *  *  * \n" +
      "P+\n" +
      "P-\n" +
      "+",
    "P1-KY-KE-GI-KI-OU *  * -KE-KY\n" +
      "P2 * -HI *  *  *  * -KI-GI * \n" +
      "P3-FU-FU-FU+UM-FU-FU * -FU * \n" +
      "P4 *  *  *  *  *  * -FU * -FU\n" +
      "P5 *  * +FU *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  * +FU\n" +
      "P7+FU+FU *  * +FU+FU+FU+FU * \n" +
      "P8 *  * +GI * +KI *  *  *  * \n" +
      "P9+KY+KE *  * +OU+KI+GI+KE+KY\n" +
      "P+00KA00FU\n" +
      "P-00HI00FU\n" +
      "-",
    "P1-NY-NK-NG-KI-OU-KI-NG-NK-NY\n" +
      "P2 * -RY *  *  *  *  * -UM * \n" +
      "P3-TO-TO-TO-TO-TO-TO-TO-TO-TO\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+TO+TO+TO+TO+TO+TO+TO+TO+TO\n" +
      "P8 * +UM *  *  *  *  * +RY * \n" +
      "P9+NY+NK+NG+KI+OU+KI+NG+NK+NY\n" +
      "P+\n" +
      "P-\n" +
      "+",
    "P1 *  *  *  * -OU *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  *  *  *  *  *  * \n" +
      "P+00HI00HI00KA00KA00KI00KI00KI00KI00GI00GI00GI00GI00KE00KE00KE00KE" +
      "00KY00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU" +
      "00FU00FU00FU00FU00FU00FU\n" +
      "P-\n" +
      "+",
    "P1 *  *  *  *  *  *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  * +OU *  *  *  * \n" +
      "P+\n" +
      "P-00HI00HI00KA00KA00KI00KI00KI00KI00GI00GI00GI00GI00KE00KE00KE00KE" +
      "00KY00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU" +
      "00FU00FU00FU00FU00FU00FU\n" +
      "-"
  )

  val csaForTestAlt = Seq(
    "PI\n+",
    "PI\n" + "P+\n" * 1000 + "P-\n" * 1000 + "+",
    "P+19KY29KE39GI49KI59OU69KI79GI89KE99KY28HI88KA17FU27FU37FU47FU57FU67FU77FU87FU97FU\n" +
      "P-11KY21KE31GI41KI51OU61KI71GI81KE91KY22KA82HI13FU23FU33FU43FU53FU63FU73FU83FU93FU\n" +
      "+",
    "P-11KY21KE31GI41KI51OU61KI71GI81KE91KY22KA82HI13FU23FU33FU43FU53FU63FU73FU83FU93FU\n" +
      "P+19KY29KE39GI49KI59OU69KI79GI89KE99KY28HI88KA17FU27FU37FU47FU57FU67FU77FU87FU97FU\n" +
      "+",
    "P-63FU\nP-73FU\nP-83FU\nP-93FU\n" +
      "P+79GI\nP+89KE\nP+99KY\nP+28HI\nP+88KA\nP+17FU\nP+27FU\nP+37FU\nP+47FU\nP+57FU\nP+67FU\nP+77FU\nP+87FU\n" +
      "P-11KY\nP-21KE\nP-31GI\n" +
      "P+19KY\nP+29KE\nP+39GI\nP+49KI\nP+59OU\nP+69KI\n" +
      "P-41KI\nP-51OU\nP-61KI\nP-71GI\nP-81KE\nP-91KY\nP-22KA\nP-82HI\nP-13FU\nP-23FU\nP-33FU\nP-43FU\nP-53FU\n" +
      "P+97FU\n" +
      "+",
    "P1 *  *  *  *  *  *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  *  *  *  *  *  * \n" +
      "P+\n" * 1000 +
      "P-\n" * 1000 +
      "+",
    "+",
    "-",
    "PI19KY29KE39GI49KI59OU69KI79GI89KE99KY28HI88KA17FU27FU37FU47FU57FU67FU77FU87FU97FU" +
      "11KY21KE31GI41KI51OU61KI71GI81KE91KY22KA82HI13FU23FU33FU43FU53FU63FU73FU83FU93FU\n+",
    "PI82HI22KA19KY\n-",
    "PI82HI\nP+55HI\n+",
    "P+59OU00FU\nP-51OU\nP+00FU\n-",
    "P+59OU00FU\nP-51OU\nP+00FU\nP+11UM\nP-12TO\nP-00AL\n-",
    "P+00AL\n-",
    "P-00AL\n-",
    "P+51OU00AL\n+",
    "P+51OU\nP-00AL\n+"
  )

  "CsaStateReader#parseInitExpression" must "work with normal cases" in {
    TestCsaStateReader.parseInitExpression("PI") must be(Some((Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS, zeroCap)))
    TestCsaStateReader.parseInitExpression("PI82HI22KA19KY") must be(Some((Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS, zeroCap ++ Map(ROOK -> 1, BISHOP -> 1, LANCE -> 1))))
    TestCsaStateReader.parseInitExpression("PI19KY29KE39GI49KI59OU69KI79GI89KE99KY28HI88KA17FU27FU37FU47FU57FU67FU77FU87FU97FU" +
      "11KY21KE31GI41KI51OU61KI71GI81KE91KY22KA82HI13FU23FU33FU43FU53FU63FU73FU83FU93FU") must be(
      Some((Map.empty, State.EMPTY_HANDS, State.capacity)))
  }
  it must "return None when in error cases" in {
    TestCsaStateReader.parseInitExpression("") mustBe None
    TestCsaStateReader.parseInitExpression(" ") mustBe None
    TestCsaStateReader.parseInitExpression("x" * 1000) mustBe None
    TestCsaStateReader.parseInitExpression("Pi") mustBe None
    TestCsaStateReader.parseInitExpression("PI2") mustBe None
    TestCsaStateReader.parseInitExpression("PI22") mustBe None
    TestCsaStateReader.parseInitExpression("PI22K") mustBe None
    TestCsaStateReader.parseInitExpression("PI22KA2") mustBe None
    TestCsaStateReader.parseInitExpression("PI10KY") mustBe None
    TestCsaStateReader.parseInitExpression("PI12KY") mustBe None
    TestCsaStateReader.parseInitExpression("PI55KA") mustBe None
    TestCsaStateReader.parseInitExpression("PI11NY") mustBe None
    TestCsaStateReader.parseInitExpression("PI22KA82HI82HI") mustBe None
    TestCsaStateReader.parseInitExpression("PI19KY29KE39GI49KI59OU69KI79GI89KE99KY28HI88KA17FU27FU37FU47FU57FU67FU77FU87FU97FU" +
      "11KY21KE31GI41KI51OU61KI71GI81KE91KY22KA82HI13FU23FU33FU43FU53FU63FU73FU83FU93FU19KY") mustBe None
  }

  "CsaStateReader#parseBundleExpression" must "work with normal cases" in {
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe Some((Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS, zeroCap))

    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * "
    )) mustBe Some((Map.empty, State.EMPTY_HANDS, State.capacity))

    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1=*  *  *  *  *  *  *  *  * ",
      "P2 * ?*  *  *  *  *  *  *  * ",
      "P3 *  * !*  *  *  *  *  *  * ",
      "P4 *  *  * 1*  *  *  *  *  * ",
      "P5 *  *  *  * a*  *  *  *  * ",
      "P6 *  *  *  *  * A*  *  *  * ",
      "P7 *  *  *  *  *  * ~*  *  * ",
      "P8 *  *  *  *  *  *  * **  * ",
      "P9 *  *  *  *  *  *  *  * )* "
    )) mustBe Some((Map.empty, State.EMPTY_HANDS, State.capacity))

    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU *  * -KE-KY",
      "P2 * -HI *  *  *  * -KI-GI * ",
      "P3-FU-FU-FU+UM-FU-FU * -FU * ",
      "P4 *  *  *  *  *  * -FU * -FU",
      "P5 *  * +FU *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  * +FU",
      "P7+FU+FU *  * +FU+FU+FU+FU * ",
      "P8 *  * +GI * +KI *  *  *  * ",
      "P9+KY+KE *  * +OU+KI+GI+KE+KY"
    )) mustBe Some((Map(
      P11 -> WL, P21 -> WN, P22 -> WS, P32 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P82 -> WR,
      P14 -> WP, P23 -> WP, P34 -> WP, P43 -> WP, P53 -> WP, P63 -> BPB, P73 -> WP, P83 -> WP, P93 -> WP,
      P16 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P75 -> BP, P87 -> BP, P97 -> BP,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P58 -> BG, P78 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS, zeroCap.updated(ROOK, 1).updated(BISHOP, 1).updated(PAWN, 2)))
  }
  it must "return None when in error cases" in {
    TestCsaStateReader.parseBundleExpression(initResult, List()) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List("")) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List.fill(9)("")) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List.fill(9)(" ")) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List.fill(9)("x" * 1000)) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE *"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY ",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE * "
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY * ",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE * "
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P2-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P2-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P1 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-F -FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+F +FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * +FU *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * -KI *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * -KA *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * +TO *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * -NG *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
    TestCsaStateReader.parseBundleExpression(initResult, List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  * -UM *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"
    )) mustBe None
  }

  "CsaStateReader#parseSingleExpression" must "work with normal cases" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+") must be(Some(initResult.get, false))
    TestCsaStateReader.parseSingleExpression(initResult, "P-") must be(Some(initResult.get, false))
    TestCsaStateReader.parseSingleExpression(hirateResult, "P+") must be(Some(hirateResult.get, false))
    TestCsaStateReader.parseSingleExpression(hirateResult, "P-") must be(Some(hirateResult.get, false))
    TestCsaStateReader.parseSingleExpression(hirateResult, "P+00AL") must be(Some(hirateResult.get, true))
    TestCsaStateReader.parseSingleExpression(hirateResult, "P-00AL") must be(Some(hirateResult.get, true))
    TestCsaStateReader.parseSingleExpression(initResult, "P+55KI") must be(Some((
      Map(P55 -> BG), State.EMPTY_HANDS, State.capacity.updated(GOLD, 3)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P-55KI") must be(Some((
      Map(P55 -> WG), State.EMPTY_HANDS, State.capacity.updated(GOLD, 3)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P+00KI") must be(Some((
      Map(), State.EMPTY_HANDS ++ Map(Hand(BG) -> 1), State.capacity.updated(GOLD, 3)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P-00KI") must be(Some((
      Map(), State.EMPTY_HANDS ++ Map(Hand(WG) -> 1), State.capacity.updated(GOLD, 3)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P+00KI55KI00KI") must be(Some((
      Map(P55 -> BG), State.EMPTY_HANDS ++ Map(Hand(BG) -> 2), State.capacity.updated(GOLD, 1)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P-00KI55KI00KI") must be(Some((
      Map(P55 -> WG), State.EMPTY_HANDS ++ Map(Hand(WG) -> 2), State.capacity.updated(GOLD, 1)), false))
    TestCsaStateReader.parseSingleExpression(initResult, "P+00AL") must be(Some((
      Map(), State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2, BR -> 2).mapKeys(Hand.apply), zeroCap), true))
    TestCsaStateReader.parseSingleExpression(initResult, "P-00AL") must be(Some((
      Map(), State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2, WR -> 2).mapKeys(Hand.apply), zeroCap), true))
    TestCsaStateReader.parseSingleExpression(initResult, "P-11TO22NY33NK44NG55KI66UM77RY88OU00FU99GI00AL") must be(Some((
      Map(
        P11 -> WPP, P22 -> WPL, P33 -> WPN, P44 -> WPS, P55 -> WG, P66 -> WPB, P77 -> WPR, P88 -> WK, P99 -> WS
      ),
      State.EMPTY_HANDS ++ Map(WP -> 17, WL -> 3, WN -> 3, WS -> 2, WG -> 3, WB -> 1, WR -> 1).mapKeys(Hand.apply),
      zeroCap
    ), true))
    TestCsaStateReader.parseSingleExpression(Some((
      Map(
        P11 -> WPP, P22 -> WPL, P33 -> WPN, P44 -> WPS, P55 -> WG, P66 -> WPB, P77 -> WPR, P88 -> WK, P99 -> WS
      ),
      State.EMPTY_HANDS ++ Map(WP -> 12, WL -> 3, WN -> 1, WS -> 0, WG -> 1, WB -> 1, WR -> 1).mapKeys(Hand.apply),
      Map(KING -> 1, ROOK -> 0, BISHOP -> 0, GOLD -> 2, SILVER -> 2, KNIGHT -> 2, LANCE -> 0, PAWN -> 5)
    )), "P+12FU13TO00GI89GI") must be(Some((
      Map(
        P11 -> WPP, P22 -> WPL, P33 -> WPN, P44 -> WPS, P55 -> WG, P66 -> WPB, P77 -> WPR, P88 -> WK, P99 -> WS,
        P12 -> BP, P13 -> BPP, P89 -> BS
      ),
      State.EMPTY_HANDS ++ Map(WP -> 12, WL -> 3, WN -> 1, WS -> 0, WG -> 1, WB -> 1, WR -> 1, BS -> 1).mapKeys(Hand.apply),
      Map(KING -> 1, ROOK -> 0, BISHOP -> 0, GOLD -> 2, SILVER -> 0, KNIGHT -> 2, LANCE -> 0, PAWN -> 3)
    ), false))
    TestCsaStateReader.parseSingleExpression(Some((
      Map(
        P11 -> WPP, P22 -> WPL, P33 -> WPN, P44 -> WPS, P55 -> WG, P66 -> WPB, P77 -> WPR, P88 -> WK, P99 -> WS
      ),
      State.EMPTY_HANDS ++ Map(WP -> 12, WL -> 3, WN -> 1, WS -> 0, WG -> 1, WB -> 1, WR -> 1).mapKeys(Hand.apply),
      Map(KING -> 1, ROOK -> 0, BISHOP -> 0, GOLD -> 2, SILVER -> 2, KNIGHT -> 2, LANCE -> 0, PAWN -> 5)
    )), "P+12FU13TO00GI89GI00AL") must be(Some((
      Map(
        P11 -> WPP, P22 -> WPL, P33 -> WPN, P44 -> WPS, P55 -> WG, P66 -> WPB, P77 -> WPR, P88 -> WK, P99 -> WS,
        P12 -> BP, P13 -> BPP, P89 -> BS
      ),
      State.EMPTY_HANDS ++ Map(WP -> 12, WL -> 3, WN -> 1, WS -> 0, WG -> 1, WB -> 1, WR -> 1, BP -> 3, BN -> 2, BG -> 2, BS -> 1).mapKeys(Hand.apply),
      zeroCap
    ), true))
  }
  it must "return None when in error cases" in {
    TestCsaStateReader.parseSingleExpression(initResult, "") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, " ") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "x" * 1000) mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+" + "x" * 1000) mustBe None
    TestCsaStateReader.parseSingleExpression(hirateResult, "x" * 1000) mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P 77FU") mustBe None
  }
  it must "return None if the length of the string is invalid" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+55F") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+55FU5") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+55FU55") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+55FU55F") mustBe None
  }
  it must "return None if a position is invalid" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+01FU") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+ABFU") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P-11TO12TO13TO10TO") mustBe None
  }
  it must "return None if a piece is invalid" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+33Fu") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+33FU342444FU") mustBe None
  }
  it must "return None if a piece on the board is duplicated" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P-33FU33FU") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+33FU33KE") mustBe None
    TestCsaStateReader.parseSingleExpression(hirateResult, "P+77FU") mustBe None
  }
  it must "return None if a piece type in hands is invalid" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+00OU") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+00TO") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+00FU00KE00NG") mustBe None
  }
  it must "return None if 00AL is not the last" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+00AL55FU") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+00AL00AL") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P-00AL00FU") mustBe None
    TestCsaStateReader.parseSingleExpression(hirateResult, "P-00AL00AL00AL") mustBe None
  }
  it must "return None if the number of a piece is over capacity" in {
    TestCsaStateReader.parseSingleExpression(initResult, "P+12KA13KA14KA") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P+12RY13HI14RY") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult, "P-00KI00KI00KI00KI00KI") mustBe None
    TestCsaStateReader.parseSingleExpression(initResult,
      "P-11FU21FU31FU41FU51FU61FU71FU81FU91FU12TO22TO32TO42TO52TO62TO72TO82TO92TO00FU") mustBe None
    TestCsaStateReader.parseSingleExpression(hirateResult, "P-55TO") mustBe None
    TestCsaStateReader.parseSingleExpression(hirateResult, "P-00FU") mustBe None
  }

  "CsaStateReader#parseCsaString" must "work in normal cases" in {
    csaForTest.map(TestCsaStateReader.parseCsaString) zip dataForTest.map(Some(_)) foreach { case (a, b) => a must be(b) }

    csaForTestAlt.map(TestCsaStateReader.parseCsaString) zip Seq(
      State.HIRATE, State.HIRATE, State.HIRATE, State.HIRATE, State.HIRATE,
      State(BLACK, Map(), State.EMPTY_HANDS),
      State(BLACK, Map(), State.EMPTY_HANDS),
      State(WHITE, Map(), State.EMPTY_HANDS),
      State(BLACK, Map(), State.EMPTY_HANDS),
      State(WHITE, Map(
        P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
        P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
        P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
        P28 -> BR, P88 -> BB,
        P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
      ), State.EMPTY_HANDS),
      State(BLACK, Map(
        P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
        P22 -> WB, P55 -> BR,
        P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
        P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
        P28 -> BR, P88 -> BB,
        P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
      ), State.EMPTY_HANDS),
      State(WHITE, Map(P59 -> BK, P51 -> WK), State.EMPTY_HANDS ++ Map(BP -> 2).mapKeys(Hand.apply)),
      State(WHITE, Map(P59 -> BK, P51 -> WK, P11 -> BPB, P12 -> WPP), State.EMPTY_HANDS ++ Map(BP -> 2, WP -> 15,
        WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 1, WR -> 2).mapKeys(Hand.apply)),
      State(WHITE, Map(), State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2, BR -> 2).mapKeys(Hand.apply)),
      State(WHITE, Map(), State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2, WR -> 2).mapKeys(Hand.apply)),
      State(BLACK, Map(P51 -> BK), State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2,
        BR -> 2).mapKeys(Hand.apply)),
      State(BLACK, Map(P51 -> BK), State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2,
        WR -> 2).mapKeys(Hand.apply))
    ).map(Some(_)) foreach { case (a, b) => a must be(b) }
  }
  it must "return None in error cases" in {
    TestCsaStateReader.parseCsaString("") mustBe None
    TestCsaStateReader.parseCsaString(" ") mustBe None
    TestCsaStateReader.parseCsaString("x" * 1000) mustBe None
    TestCsaStateReader.parseCsaString("Pi\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI10KY\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI12KY\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI11NY\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI22KA82HI82HI\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI\nP+55KA\n+") mustBe None
    TestCsaStateReader.parseCsaString("PI55KA\n00AL55KA\n+") mustBe None
    TestCsaStateReader.parseCsaString("P+11OU12OU\n+") mustBe None
    TestCsaStateReader.parseCsaString("P-11OU12OU\n+") mustBe None

    // two kings in the same side
    TestCsaStateReader.parseCsaString(Seq(
      "P1-KY-KE-GI-KI+OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "+"
    ).mkString("\n")) mustBe None
    TestCsaStateReader.parseCsaString(Seq(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI-OU+KI+GI+KE+KY",
      "+"
    ).mkString("\n")) mustBe None
    TestCsaStateReader.parseCsaString("P+55OU54OU\n+") mustBe None
    TestCsaStateReader.parseCsaString("P+55OU54OU53OU\n+") mustBe None
    TestCsaStateReader.parseCsaString("P-55OU54OU\n+") mustBe None
    TestCsaStateReader.parseCsaString("P-51OU\nP-58KI59OU\n+") mustBe None

    // able to attack the opponent's king
    TestCsaStateReader.parseCsaString(Seq(
      "P1-KY-KE-GI-KI+OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU * -FU-FU-FU-FU-FU-FU",
      "P4 *  * -FU *  *  *  *  *  * ",
      "P5+KA *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 *  *  *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "+"
    ).mkString("\n")) mustBe None
    TestCsaStateReader.parseCsaString(Seq(
      "P1-KY-KE-GI-KI+OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  *  *  * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  * -KA",
      "P6 *  *  *  *  *  * +FU *  * ",
      "P7+FU+FU+FU+FU+FU+FU * +FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "+"
    ).mkString("\n")) mustBe None
  }

}
