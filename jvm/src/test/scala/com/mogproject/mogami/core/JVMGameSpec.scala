package com.mogproject.mogami.core

import com.mogproject.mogami._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class JVMGameSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  def loadFile(path: String, encoding: String = "utf-8"): String = {
    scala.io.Source.fromFile(s"jvm/src/test/resources/${path}", encoding).mkString.replace("\r", "")
  }

  "Game#parseCsaString" must "create games from files" in {
    Game.parseCsaString(loadFile("csa/game/001.csa")).moves.length mustBe 111
  }

  "Game#parseKifString" must "create games from files" in {
    Game.parseKifString(loadFile("kif/game/001.kif")).moves.length mustBe 111
    Game.parseKifString(loadFile("kif/game/002.kif")).moves.length mustBe 193
    Game.parseKifString(loadFile("kif/game/003.kif", "sjis")).moves.length mustBe 117
    Game.parseKifString(loadFile("kif/game/004.kif")).moves.length mustBe 223
  }

  "Game#parseKi2String" must "create games from files" in {
    Game.parseKi2String(loadFile("ki2/game/001.ki2")).moves.length mustBe 111
    Game.parseKi2String(loadFile("ki2/game/002.ki2")).moves.length mustBe 111
  }
}
