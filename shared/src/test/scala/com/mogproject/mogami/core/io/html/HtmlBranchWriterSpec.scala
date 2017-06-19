package com.mogproject.mogami.core.io.html

import com.mogproject.mogami.core.game.Branch
import com.mogproject.mogami.core.state.{State, StateCache}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class HtmlBranchWriterSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "HtmlBranchWriter#toHtmlString" must "make html with comments" in {
    StateCache.withCache { implicit cache =>
      val br = Branch(State.HIRATE)
      br.toHtmlString(isJapanese = true, Map(br.historyHash.head -> "<comment&123>")) mustBe
        "<div class=\"shogi-state\"><div class=\"shogi-header\">Start</div><div class=\"shogi-body\"><table><tbody><tr><td><div class=\"shogi-hand\">☖</div>" +
          "<table class=\"shogi-board\"><tbody><tr><td class=\"sb-f\">9</td><td class=\"sb-f\">8</td><td class=\"sb-f\">7</td>" +
          "<td class=\"sb-f\">6</td><td class=\"sb-f\">5</td><td class=\"sb-f\">4</td><td class=\"sb-f\">3</td><td class=\"sb-f\">2</td>" +
          "<td class=\"sb-f\">1</td><td class=\"sb-f sb-r\"/></tr><tr><td class=\"sp-i\">香</td><td class=\"sp-i\">桂</td>" +
          "<td class=\"sp-i\">銀</td><td class=\"sp-i\">金</td><td class=\"sp-i\">玉</td><td class=\"sp-i\">金</td><td class=\"sp-i\">銀</td>" +
          "<td class=\"sp-i\">桂</td><td class=\"sp-i\">香</td><td class=\"sb-r\">1</td></tr><tr><td/><td class=\"sp-i\">飛</td><td/><td/>" +
          "<td/><td/><td/><td class=\"sp-i\">角</td><td/><td class=\"sb-r\">2</td></tr><tr><td class=\"sp-i\">歩</td>" +
          "<td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td>" +
          "<td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sb-r\">3</td></tr><tr><td/><td/><td/>" +
          "<td/><td/><td/><td/><td/><td/><td class=\"sb-r\">4</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/>" +
          "<td class=\"sb-r\">5</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">6</td></tr><tr><td>歩</td>" +
          "<td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td class=\"sb-r\">7</td></tr><tr><td/>" +
          "<td>角</td><td/><td/><td/><td/><td/><td>飛</td><td/><td class=\"sb-r\">8</td></tr><tr><td>香</td><td>桂</td><td>銀</td><td>金</td>" +
          "<td>玉</td><td>金</td><td>銀</td><td>桂</td><td>香</td><td class=\"sb-r\">9</td></tr>" +
          "</tbody></table><div class=\"shogi-hand\">☗</div></td><td class=\"shogi-comment\"><p>&lt;comment&amp;123&gt;</p></td></tr></tbody></table></div></div>\n"
    }
  }
}
