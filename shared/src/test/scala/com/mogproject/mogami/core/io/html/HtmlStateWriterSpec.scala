package com.mogproject.mogami.core.io.html

import com.mogproject.mogami.core.state.State.{BoardType, HandType}
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.state.StateConstant._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.state.StateGen
import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HtmlStateWriterSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  case class TestHtmlStateWriter(board: BoardType, hand: HandType) extends HtmlStateWriter

  "HtmlStateWriter#toHtmlString" must "set reflect the position" in {
    TestHtmlStateWriter(HIRATE.board, HIRATE.hand).toHtmlString() mustBe
      "<div class=\"shogi-state\"><div class=\"shogi-header\"></div><div class=shogi-body><div class=\"shogi-hand\">☖</div>" +
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
        "</tbody></table><div class=\"shogi-hand\">☗</div></div></div>"

    TestHtmlStateWriter(MATING_BLACK.board, MATING_BLACK.hand).toHtmlString() mustBe "<div class=\"shogi-state\"><div class=\"shogi-header\"></div><div class=shogi-body><div class=\"shogi-hand\">☖飛2角2金4銀4桂4香4歩18</div><table class=\"shogi-board\"><tbody><tr><td class=\"sb-f\">9</td><td class=\"sb-f\">8</td><td class=\"sb-f\">7</td><td class=\"sb-f\">6</td><td class=\"sb-f\">5</td><td class=\"sb-f\">4</td><td class=\"sb-f\">3</td><td class=\"sb-f\">2</td><td class=\"sb-f\">1</td><td class=\"sb-f sb-r\"/></tr><tr><td/><td/><td/><td/><td class=\"sp-i\">玉</td><td/><td/><td/><td/><td class=\"sb-r\">1</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">2</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">3</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">4</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">5</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">6</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">7</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">8</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">9</td></tr></tbody></table><div class=\"shogi-hand\">☗</div></div></div>"
    TestHtmlStateWriter(MATING_WHITE.board, MATING_WHITE.hand).toHtmlString() mustBe "<div class=\"shogi-state\"><div class=\"shogi-header\"></div><div class=shogi-body><div class=\"shogi-hand\">☖</div><table class=\"shogi-board\"><tbody><tr><td class=\"sb-f\">9</td><td class=\"sb-f\">8</td><td class=\"sb-f\">7</td><td class=\"sb-f\">6</td><td class=\"sb-f\">5</td><td class=\"sb-f\">4</td><td class=\"sb-f\">3</td><td class=\"sb-f\">2</td><td class=\"sb-f\">1</td><td class=\"sb-f sb-r\"/></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">1</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">2</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">3</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">4</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">5</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">6</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">7</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">8</td></tr><tr><td/><td/><td/><td/><td>玉</td><td/><td/><td/><td/><td class=\"sb-r\">9</td></tr></tbody></table><div class=\"shogi-hand\">☗飛2角2金4銀4桂4香4歩18</div></div></div>"
  }
  it must "reflect the header" in {
    TestHtmlStateWriter(HIRATE.board, HIRATE.hand).toHtmlString("Header header") mustBe
      "<div class=\"shogi-state\"><div class=\"shogi-header\">Header header</div><div class=shogi-body><div class=\"shogi-hand\">☖</div>" +
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
        "</tbody></table><div class=\"shogi-hand\">☗</div></div></div>"
  }
  it must "reflect last moves" in {
    TestHtmlStateWriter(HIRATE.board, HIRATE.hand).toHtmlString(lastMove = Some(P33)) mustBe
      "<div class=\"shogi-state\"><div class=\"shogi-header\"></div><div class=shogi-body><div class=\"shogi-hand\">☖</div>" +
        "<table class=\"shogi-board\"><tbody><tr><td class=\"sb-f\">9</td><td class=\"sb-f\">8</td><td class=\"sb-f\">7</td>" +
        "<td class=\"sb-f\">6</td><td class=\"sb-f\">5</td><td class=\"sb-f\">4</td><td class=\"sb-f\">3</td><td class=\"sb-f\">2</td>" +
        "<td class=\"sb-f\">1</td><td class=\"sb-f sb-r\"/></tr><tr><td class=\"sp-i\">香</td><td class=\"sp-i\">桂</td>" +
        "<td class=\"sp-i\">銀</td><td class=\"sp-i\">金</td><td class=\"sp-i\">玉</td><td class=\"sp-i\">金</td><td class=\"sp-i\">銀</td>" +
        "<td class=\"sp-i\">桂</td><td class=\"sp-i\">香</td><td class=\"sb-r\">1</td></tr><tr><td/><td class=\"sp-i\">飛</td><td/><td/>" +
        "<td/><td/><td/><td class=\"sp-i\">角</td><td/><td class=\"sb-r\">2</td></tr><tr><td class=\"sp-i\">歩</td>" +
        "<td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td>" +
        "<td class=\"sp-i sp-b\">歩</td><td class=\"sp-i\">歩</td><td class=\"sp-i\">歩</td><td class=\"sb-r\">3</td></tr><tr><td/><td/><td/>" +
        "<td/><td/><td/><td/><td/><td/><td class=\"sb-r\">4</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/>" +
        "<td class=\"sb-r\">5</td></tr><tr><td/><td/><td/><td/><td/><td/><td/><td/><td/><td class=\"sb-r\">6</td></tr><tr><td>歩</td>" +
        "<td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td>歩</td><td class=\"sb-r\">7</td></tr><tr><td/>" +
        "<td>角</td><td/><td/><td/><td/><td/><td>飛</td><td/><td class=\"sb-r\">8</td></tr><tr><td>香</td><td>桂</td><td>銀</td><td>金</td>" +
        "<td>玉</td><td>金</td><td>銀</td><td>桂</td><td>香</td><td class=\"sb-r\">9</td></tr>" +
        "</tbody></table><div class=\"shogi-hand\">☗</div></div></div>"
  }
  it must "has 100 td elements" in forAll(StateGen.statesWithFullPieces, minSuccessful(10)) { st =>
    TestHtmlStateWriter(st.board, st.hand).toHtmlString().split("<td").length mustBe 101
  }
}
