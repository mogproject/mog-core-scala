package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.state.State
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}


class SpecialMoveSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "IllegalMove#toJapaneseNotationString" must "return move expression" in {
    IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get).toJapaneseNotationString mustBe "７六歩"
  }

  "IllegalMove#toWestern" must "return move expression" in {
    IllegalMove(MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get).toWesternNotationString mustBe "P-7f"
  }

}
