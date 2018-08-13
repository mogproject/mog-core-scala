package com.mogproject.mogami.core.io.html

import com.mogproject.mogami.core.Player.BLACK
import com.mogproject.mogami.core.Ptype.KING
import com.mogproject.mogami.core.SquareConstant.{P51, P59}
import com.mogproject.mogami.core.game.Branch
import com.mogproject.mogami.core.move.{IllegalMove, Move}
import com.mogproject.mogami.core.state.StateConstant.HIRATE
import com.mogproject.mogami.core.state.{State, StateCache}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class HtmlBranchWriterSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "HtmlBranchWriter#toHtmlString" must "make html with comments" in {
    StateCache.withCache { implicit cache =>
      val br = Branch(State.HIRATE)
      br.toHtmlString(isJapanese = true, Map(br.historyHash.head -> "<comment&123>")) mustBe
        "<div class=\"shogi-state\"><div class=\"shogi-header\">初期局面</div><div class=\"shogi-body\"><table><tbody><tr><td><div class=\"shogi-hand\">☖</div>" +
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
          "</tbody></table><div class=\"shogi-hand\">☗</div></td><td class=\"shogi-comment\"><p>&lt;comment&amp;123&gt;</p></td></tr></tbody></table></div></div>"
    }
  }
  it must "insert a table" in {
    StateCache.withCache { implicit cache =>
      val br = Branch(State.HIRATE)
      br.toHtmlString(isJapanese = true, Map(br.historyHash.head -> "<comment&123>"), Some(2)) mustBe
        "<table class=\"shogi-page\"><tbody>\n<tr><td colspan=\"2\"><div class=\"shogi-state\"><div class=\"shogi-header\">初期局面</div><div class=\"shogi-body\"><table><tbody><tr><td><div class=\"shogi-hand\">☖</div>" +
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
          "</tbody></table><div class=\"shogi-hand\">☗</div></td><td class=\"shogi-comment\"><p>&lt;comment&amp;123&gt;</p></td></tr></tbody></table></div></div>" +
          "</td></tr>\n\n</tbody></table>"
    }
  }
  it must "handle illegal moves" in {
    StateCache.withCache { implicit cache =>
      val br = Branch(cache.set(HIRATE), 0, Vector.empty, Some(IllegalMove(
        Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)
      )))
      br.toHtmlString(isJapanese = true, Map(br.historyHash.head -> "<comment&123>"), Some(2)) mustBe
        "<table class=\"shogi-page\"><tbody>\n<tr><td colspan=\"2\"><div class=\"shogi-state\"><div class=\"shogi-header\">初期局面</div><div class=\"shogi-body\"><table><tbody><tr><td><div class=\"shogi-hand\">☖</div>" +
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
          "</tbody></table><div class=\"shogi-hand\">☗</div></td><td class=\"shogi-comment\"><p>&lt;comment&amp;123&gt;</p></td></tr></tbody></table></div></div>" +
          "</td></tr>\n"+
          """<tr><td><div class="shogi-state"><div class="shogi-header">反則手: ☗５一玉</div><div class="shogi-body"><table><tbody><tr><td><div class="shogi-hand">☖</div><table class="shogi-board"><tbody><tr><td class="sb-f">9</td><td class="sb-f">8</td><td class="sb-f">7</td><td class="sb-f">6</td><td class="sb-f">5</td><td class="sb-f">4</td><td class="sb-f">3</td><td class="sb-f">2</td><td class="sb-f">1</td><td class="sb-f sb-r"/></tr><tr><td class="sp-i">香</td><td class="sp-i">桂</td><td class="sp-i">銀</td><td class="sp-i">金</td><td class="sp-b">玉</td><td class="sp-i">金</td><td class="sp-i">銀</td><td class="sp-i">桂</td><td class="sp-i">香</td><td class="sb-r">1</td></tr><tr><td/><td class="sp-i">飛</td><td/><td/><td/><td/><td/><td class="sp-i">角</td><td/><td class="sb-r">2</td></tr><tr><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sp-i">歩</td><td class="sb-r">3</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class="sb-r">4</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class="sb-r">5</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class="sb-r">6</td></tr><tr><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td class="sb-r">7</td></tr><tr><td/><td>角</td><td/><td/><td/><td/><td/><td>飛</td><td/><td class="sb-r">8</td></tr><tr><td>香</td><td>桂</td><td>銀</td><td>金</td><td/><td>金</td><td>銀</td><td>桂</td><td>香</td><td class="sb-r">9</td></tr></tbody></table><div class="shogi-hand">☗</div></td><td class="shogi-comment"><p></p></td></tr></tbody></table></div></div></td></tr>""" +
          "\n</tbody></table>"
    }
  }
}
