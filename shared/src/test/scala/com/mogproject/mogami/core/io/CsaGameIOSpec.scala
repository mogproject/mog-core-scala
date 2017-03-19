package com.mogproject.mogami.core.io

import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.StateConstant.HIRATE
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign, TimeUp}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class CsaGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestCsaGameReader extends CsaGameReader

  "CsaGameReader#parseMovesCsa" must "parse normal moves" in {
    TestCsaGameReader.parseMovesCsa(List(), None, Some(Game())) mustBe Some(Game())
  }
  it must "parse special moves" in {
    TestCsaGameReader.parseMovesCsa(List("%TORYO"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(Resign())))
    TestCsaGameReader.parseMovesCsa(List("%TORYO,T123"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(Resign(Some(123)))))
    TestCsaGameReader.parseMovesCsa(List("%TIME_UP"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(TimeUp())))
    TestCsaGameReader.parseMovesCsa(List("%TIME_UP,T123"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(TimeUp(Some(123)))))
    TestCsaGameReader.parseMovesCsa(List("+5951OU", "%ILLEGAL_MOVE"), None, Some(Game())) mustBe Some(Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)))
    ))
    TestCsaGameReader.parseMovesCsa(List("+5951OU,T123", "%ILLEGAL_MOVE"), None, Some(Game())) mustBe Some(Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)))
    ))
  }

}
