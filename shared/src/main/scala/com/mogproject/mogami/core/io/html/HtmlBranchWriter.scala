package com.mogproject.mogami.core.io.html

import com.mogproject.mogami.core.game.Game.{CommentType, HistoryHash}
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.State.{BoardType, HandType}
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.util.Implicits._

/**
  * one branch to html
  */
trait HtmlBranchWriter {
  def offset: Int

  def moves: Vector[Move]

  def history: Vector[StateHash]

  def historyHash: Vector[HistoryHash]

  def finalAction: Option[SpecialMove]

  private[this] def moveToString(move: Move, isJapanese: Boolean): String =
    move.player.toSymbolString() + isJapanese.fold(move.toJapaneseNotationString, move.toWesternNotationString)

  private[this] def finalActionToString(isJapanese: Boolean): String =
    finalAction.map(m => isJapanese.fold(m.toJapaneseNotationString, m.toWesternNotationString)).getOrElse("")

  private[this] def finalActionToHtml(lastState: Option[State], isJapanese: Boolean): String =
    (finalAction, lastState) match {
      case (Some(IllegalMove(imv)), Some(st)) =>
        val header = finalActionToString(isJapanese) + ": " + moveToString(imv, isJapanese)
        val (newBoard, newHand) = st.makeNextPosition(imv)
        new HtmlStateWriter {
          override val board: BoardType = newBoard
          override val hand: HandType = newHand
        }.toHtmlString(header, Some(imv.to))
      case (Some(_), Some(st)) => st.toHtmlString(finalActionToString(isJapanese), None)
      case _ => ""
    }

  def toHtmlString(isJapanese: Boolean = true, comments: CommentType)(implicit stateCache: StateCache): String = {
    val stateHistory = history.map(stateCache.get).zip(historyHash)
    val states = stateHistory.zip(None +: moves.map(Some.apply)).zipWithIndex.map {
      case (((Some(st), hh), Some(m)), i) => st.toHtmlString(s"#${offset + i}: ${moveToString(m, isJapanese)}", Some(m.to), comments.get(hh))
      case (((Some(st), hh), None), _) => st.toHtmlString(s"Start", None, comments.get(hh))
      case _ => ""
    }
    (states :+ finalActionToHtml(stateHistory.last._1, isJapanese)).mkString("\n")
  }
}
