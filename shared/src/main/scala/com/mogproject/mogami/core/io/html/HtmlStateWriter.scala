package com.mogproject.mogami.core.io.html

import com.mogproject.mogami.{BoardType, HandType}
import com.mogproject.mogami.core.{Hand, Player, Square}
import com.mogproject.mogami.util.Implicits._

/**
  * Create a simple HTML view
  *
  * @see CSS required. Example: https://github.com/mogproject/mog-playground/tree/master/css/notesview.css
  */
trait HtmlStateWriter {
  def board: BoardType

  def hand: HandType

  private object ClassName {
    val shogiState = "shogi-state"
    val shogiHeader = "shogi-header"
    val shogiBody = "shogi-body"
    val shogiHand = "shogi-hand"
    val shogiBoard = "shogi-board"
    val shogiComment = "shogi-comment"
    val fileIndex = "sb-f"
    val rankIndex = "sb-r"
    val pieceInverted = "sp-i"
    val piecePromoted = "sp-p"
    val pieceBold = "sp-b"
  }

  import ClassName._

  private[this] def createHandString(player: Player): String = {
    player.toSymbolString() + hand.filter(h => h._1.owner == player && h._2 > 0).toSeq.sortBy(_._1.ptype.sortId).map {
      case (Hand(_, ptype), n) => ptype.toJapaneseSimpleName + (n > 1).fold(n, "")
    }.mkString
  }

  private[this] def createBoardHtml(lastMove: Option[Square]): String = {
    val horizontalIndex = (9 to 1 by -1).map(f => s"""<td class="${fileIndex}">${f}</td>""").mkString("", "", s"""<td class="${fileIndex} ${rankIndex}"/>""")
    val brd = (1 to 9).map { r =>
      (9 to 1 by -1).map { f =>
        val sq = Square(f, r)
        board.get(sq).map { p =>
          val cls = Seq((p.isPromoted, piecePromoted), (p.owner.isWhite, pieceInverted), (lastMove.contains(sq), pieceBold)).flatMap { case (b, s) => b.option(s) }
          val clsString = cls.isEmpty.fold("", cls.mkString(" class=\"", " ", "\""))
          s"""<td${clsString}>${p.ptype.toJapaneseSimpleName}</td>"""
        }.getOrElse("<td/>")
      }.mkString("") +
        s"""<td class="${rankIndex}">${r}</td>"""
    }
    (horizontalIndex +: brd).mkString("<tr>", "</tr><tr>", "</tr>")
  }

  def toHtmlString(header: String = "", lastMove: Option[Square] = None, comment: Option[String] = None): String = Seq(
    s"""<div class="${shogiState}">""",
    s"""<div class="${shogiHeader}">${header}</div>""",
    s"""<div class="${shogiBody}">""",
    comment.isDefined.fold("""<table><tbody><tr><td>""", ""),
    s"""<div class="${shogiHand}">${createHandString(Player.WHITE)}</div>""",
    s"""<table class="${shogiBoard}"><tbody>${createBoardHtml(lastMove)}</tbody></table>""",
    s"""<div class="${shogiHand}">${createHandString(Player.BLACK)}</div>""",
    comment.map(c => s"""</td><td class="${shogiComment}"><p>${escape(c)}</p></td></tr></tbody></table>""").getOrElse(""),
    """</div></div>"""
  ).mkString

  /**
    * Code to escape text HTML nodes. Taken from scala.xml
    */
  private[this] def escape(text: String, s: StringBuilder): Unit = {
    // Implemented per XML spec:
    // http://www.w3.org/International/questions/qa-controls
    // imperative code 3x-4x faster than current implementation
    // dpp (David Pollak) 2010/02/03
    val len = text.length
    var pos = 0

    while (pos < len) {
      text.charAt(pos) match {
        case '<' => s.append("&lt;")
        case '>' => s.append("&gt;")
        case '&' => s.append("&amp;")
        case '"' => s.append("&quot;")
        case '\n' => s.append('\n')
        case '\r' => s.append('\r')
        case '\t' => s.append('\t')
        case c if c < ' ' =>
        case c => s.append(c)
      }
      pos += 1
    }
  }

  private[this] def escape(text: String): String = {
    val builder = new StringBuilder
    escape(text, builder)
    builder.result()
  }
}
