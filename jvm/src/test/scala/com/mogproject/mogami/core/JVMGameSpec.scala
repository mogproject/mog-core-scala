package com.mogproject.mogami.core

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateCache
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class JVMGameSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  def loadFile(path: String, encoding: String = "utf-8"): String = {
    scala.io.Source.fromFile(s"jvm/src/test/resources/${path}", encoding).mkString.replace("\r", "")
  }

  "Game#parseCsaString" must "create games from files" in StateCache.withCache { implicit cache =>
    Game.parseCsaString(loadFile("csa/game/001.csa")).trunk.moves.length mustBe 111
    val g002 = Game.parseCsaString(loadFile("csa/game/002.csa"))
    g002.trunk.moves.length mustBe 258
    g002.trunk.status mustBe GameStatus.Jishogi
  }

  "Game#parseKifString" must "create games from files" in StateCache.withCache { implicit cache =>
    Game.parseKifString(loadFile("kif/game/001.kif")).trunk.moves.length mustBe 111
    Game.parseKifString(loadFile("kif/game/002.kif")).trunk.moves.length mustBe 193

    val g003: Game = Game.parseKifString(loadFile("kif/game/003.kif", "sjis"))
    g003.trunk.moves.length mustBe 117
    g003.branches.map(_.offset) mustBe Seq(69, 69, 49, 33)
    g003.branches.map(_.moves.length) mustBe Seq(9, 7, 13, 9)
    g003.hasComment(GamePosition(0, 0)) mustBe true
    g003.comments.size mustBe 27
    g003.comments.keySet mustBe Set(
      g003.trunk.historyHash(0),
      g003.trunk.historyHash(32),
      g003.trunk.historyHash(33),
      g003.trunk.historyHash(46),
      g003.trunk.historyHash(49),
      g003.trunk.historyHash(56),
      g003.trunk.historyHash(64),
      g003.trunk.historyHash(69),
      g003.trunk.historyHash(72),
      g003.trunk.historyHash(76),
      g003.trunk.historyHash(83),
      g003.trunk.historyHash(100),
      g003.trunk.historyHash(105),
      g003.trunk.historyHash(109),
      g003.trunk.historyHash(111),
      g003.trunk.historyHash(113),
      g003.trunk.historyHash(117),
      g003.branches(0).getHistoryHash(70).get,
      g003.branches(0).getHistoryHash(71).get,
      g003.branches(0).getHistoryHash(78).get,
      g003.branches(1).getHistoryHash(72).get,
      g003.branches(1).getHistoryHash(76).get,
      g003.branches(2).getHistoryHash(52).get,
      g003.branches(2).getHistoryHash(53).get,
      g003.branches(2).getHistoryHash(54).get,
      g003.branches(2).getHistoryHash(62).get,
      g003.branches(3).getHistoryHash(42).get
    )
    g003.comments(g003.branches(2).historyHash(3)) mustBe Seq(
      "[Taichi_NAKAMURA] 次に53銀打の狙いがあります。",
      "\"You are threatening Silver drop to 53 ",
      "here\""
    ).mkString("\n")
    g003.comments(g003.branches(2).historyHash(4)) mustBe "[Archon] I was worried about bishop drop 77"
    g003.comments(g003.branches(2).historyHash(5)) mustBe Seq(
      "[Archon] I see, and without king to defend from ",
      "earliermove",
      "( \"なるほど、本譜のように玉が守っていないので。 \")",
      "",
      "[Taichi_NAKAMURA] そうですね。実戦の進行は上手の駒の連結が良くなってしまいました。",
      "\"Yes. In the game, my pieces were connected ",
      "tighter.\"",
      ""
    ).mkString("\n")
    g003.comments(g003.branches(2).historyHash(13)) mustBe Seq(
      "[Taichi_NAKAMURA] 駒の働きや連結を意識するともっと上達すると思います。",
      "\"You will improve a lot more, if you think ",
      "more about pieces' connection and their efficiencies.\""
    ).mkString("\n")

    // toKifString can change the branch order
    Game.parseKifString(g003.toKifString).toUsenString.sum mustBe g003.toUsenString.sum
    Game.parseKifString(g003.toKifString).toUsenString mustBe Game.parseKifString(Game.parseKifString(g003.toKifString).toKifString).toUsenString
    Game.parseKifString(g003.toKifString).toKifString mustBe Game.parseKifString(Game.parseKifString(g003.toKifString).toKifString).toKifString

    Game.parseKifString(loadFile("kif/game/004.kif")).trunk.moves.length mustBe 223

    val g005: Game = Game.parseKifString(loadFile("kif/game/005.kif"))
    g005.branches.map(_.status) mustBe Seq(GameStatus.Playing, GameStatus.Mated, GameStatus.Resigned)

    Game.parseKifString(loadFile("kif/game/006.kif")).toKifString mustBe Seq(
      "手合割：平手",
      "先手：",
      "後手：",
      "",
      "手数----指手----消費時間--",
      "   1 ３六歩(37)",
      "   2 ４二玉(51)",
      "   3 ３五歩(36)",
      "   4 ３二玉(42)",
      "   5 ３四歩(35)",
      "   6 ４二銀(31)",
      "   7 ３三歩成(34)",
      "   8 同　桂(21)",
      "",
      "",
      "変化：7手",
      "   7 ３三歩成(34)",
      "",
      "",
      "変化：5手",
      "   5 ３四歩(35)",
      "",
      "",
      "変化：3手",
      "   3 ３五歩(36)",
      "",
      "",
      "変化：1手",
      "   1 ３六歩(37)",
      ""
    ).mkString("\n")

    val s7 = loadFile("kif/game/007.kif")
    Game.parseKifString(s7).toKifString mustBe s7

    val s8 = loadFile("kif/game/008.kif")
    Game.parseKifString(s8).trunk.moves.length mustBe 168

    // Sennichite
    val s9 = loadFile("kif/game/009.kif")
    val g009 = Game.parseKifString(s9)
    g009.trunk.moves.length mustBe 85
    g009.trunk.status mustBe GameStatus.Drawn

    // Jishogi
    val s10 = loadFile("kif/game/010.kif")
    val g010 = Game.parseKifString(s10)
    g010.trunk.moves.length mustBe 258
    g010.trunk.status mustBe GameStatus.Jishogi

    val m1 = loadFile("kif/mate/001.kif")
    Game.parseKifString(m1).trunk.moves.length mustBe 59

    // Kif including Zenkaku spaces
    val s11 = loadFile("kif/game/011.kif", "sjis")
    val g011 = Game.parseKifString(s11)
    g011.trunk.moves.length mustBe 114

    // Pause after Resign
    val s12 = loadFile("kif/game/012.kif", "sjis")
    val g012 = Game.parseKifString(s12)
    g012.trunk.moves.length mustBe 121

    // Shogidroid format: includes short (not-Zenkaku) spaces for initial hand pieces
    val s13 = loadFile("kif/game/013.kif", "sjis")
    val g013 = Game.parseKifString(s13)
    g013.trunk.moves.length mustBe 0
  }

  "Game#parseKi2String" must "create games from files" in StateCache.withCache { implicit cache =>
    Game.parseKi2String(loadFile("ki2/game/001.ki2")).trunk.moves.length mustBe 111
    Game.parseKi2String(loadFile("ki2/game/002.ki2")).trunk.moves.length mustBe 111
    Game.parseKi2String(loadFile("ki2/game/003.ki2")).trunk.moves.length mustBe 34
    Game.parseKi2String(loadFile("ki2/game/004.ki2")).trunk.moves.length mustBe 27
    val g005 = Game.parseKi2String(loadFile("ki2/game/005.ki2"))
    g005.trunk.moves.length mustBe 258
    g005.trunk.status mustBe GameStatus.Jishogi
  }
}
