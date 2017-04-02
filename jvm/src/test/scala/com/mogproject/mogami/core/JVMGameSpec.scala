package com.mogproject.mogami.core

import com.mogproject.mogami._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class JVMGameSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "Game#parseCsaString" must "create games from files" in {
    Game.parseCsaString(scala.io.Source.fromFile("jvm/src/test/resources/csa/game/001.csa").mkString).moves.length mustBe 111
  }

  "Game#parseKifString" must "create games from files" in {
    Game.parseKifString(scala.io.Source.fromFile("jvm/src/test/resources/kif/game/001.kif").mkString).moves.length mustBe 111
  }

  "Game#parseKi2String" must "create games from files" in {
    Game.parseKi2String(scala.io.Source.fromFile("jvm/src/test/resources/ki2/game/001.ki2").mkString).moves.length mustBe 111
  }
}
