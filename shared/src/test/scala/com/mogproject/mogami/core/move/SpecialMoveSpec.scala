package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.state.State
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers


class SpecialMoveSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "IllegalMove#toJapaneseNotationString" must "return move expression" in {
    IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get).toJapaneseNotationString mustBe "７六歩"
  }

  "IllegalMove#toWestern" must "return move expression" in {
    IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get).toWesternNotationString mustBe "P-7f"
  }

  "IllegalMove#getElapsedTime" must "return elapsed time" in {
    IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get.copy(elapsedTime = None)).getElapsedTime mustBe None
    val m = IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get.copy(elapsedTime = Some(123)))
    m.getElapsedTime mustBe Some(123)
    m.dropElapsedTime.getElapsedTime mustBe None
  }

  "Resign#getElapsedTime" must "return elapsed time" in {
    Resign(None).getElapsedTime mustBe None
    Resign(Some(123)).getElapsedTime mustBe Some(123)
    Resign(Some(123)).dropElapsedTime.getElapsedTime mustBe None
  }

  "TimeUp#getElapsedTime" must "return elapsed time" in {
    TimeUp(None).getElapsedTime mustBe None
    TimeUp(Some(123)).getElapsedTime mustBe Some(123)
    TimeUp(Some(123)).dropElapsedTime.getElapsedTime mustBe None
  }

  "Pause#getElapsedTime" must "return None" in {
    Pause.getElapsedTime mustBe None
    Pause.dropElapsedTime.getElapsedTime mustBe None
  }

  "DeclareWin#getElapsedTime" must "return None" in {
    DeclareWin(None).getElapsedTime mustBe None
    DeclareWin(Some(123)).getElapsedTime mustBe Some(123)
    DeclareWin(Some(123)).dropElapsedTime.getElapsedTime mustBe None
  }
}
