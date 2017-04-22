package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.{Player, Square}
import com.mogproject.mogami.core.io.sfen.SfenExtendedLike
import com.mogproject.mogami.core.io.{CsaLike, KifLike, RecordFormatException}
import com.mogproject.mogami.core.state.State

sealed trait SpecialMove extends CsaLike with KifLike with SfenExtendedLike {
  def toJapaneseNotationString: String

  def toWesternNotationString: String

  def toKi2String(currentPlayer: Player, numMoves: Int): String

  protected def makeKi2String(numMoves: Int, description: String): String = s"まで${numMoves}手で${description}"
}

object SpecialMove {
  def parseSfenExtended(s: String, lastState: State, lastMoveTo: Option[Square]): SpecialMove = s.split(" ").toList match {
    case Resign.sfenExtendedKeyword :: Nil => Resign()
    case TimeUp.sfenExtendedKeyword :: Nil => TimeUp()
    case Pause.sfenExtendedKeyword :: Nil => Pause
    case IllegalMove.sfenExtendedKeyword :: mv :: Nil =>
      IllegalMove(MoveBuilderSfen.parseSfenString(mv).toMove(lastState, lastMoveTo, isStrict=false).getOrElse(
        throw new RecordFormatException(1, s"invalid illegal move: ${mv}")
      ))
    case _ => throw new RecordFormatException(1, s"unknown final action: ${s}")
  }
}

/**
  * Illegal move
  *
  * @param move illegal move
  */
case class IllegalMove(move: Move) extends SpecialMove {
  override def toCsaString: String = IllegalMove.csaKeyword

  override def toKifString: String = IllegalMove.kifKeyword

  override def toSfenExtendedString: String = IllegalMove.sfenExtendedKeyword + " " + move.toSfenString

  override def toJapaneseNotationString: String = IllegalMove.kifKeyword

  override def toWesternNotationString: String = "Illegal Move"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の${IllegalMove.ki2Keyword}勝ち")
}

object IllegalMove {
  val csaKeyword = "%ILLEGAL_MOVE"
  val kifKeyword = "反則手"
  val ki2Keyword = "反則"
  val sfenExtendedKeyword = "i"
}

/**
  * Resign
  *
  * @param elapsedTime elapsed time
  */
case class Resign(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = Resign.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = Resign.kifKeyword + timeToKifString(elapsedTime)

  override def toSfenExtendedString: String = Resign.sfenExtendedKeyword

  override def toJapaneseNotationString: String = Resign.kifKeyword

  override def toWesternNotationString: String = "Resign"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の勝ち")
}

object Resign {
  val csaKeyword = "%TORYO"
  val kifKeyword = "投了"
  val sfenExtendedKeyword = "r"
}

/**
  * Time up
  *
  * @param elapsedTime elapsed time
  */
case class TimeUp(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = TimeUp.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = TimeUp.kifKeyword + timeToKifString(elapsedTime)

  override def toSfenExtendedString: String = TimeUp.sfenExtendedKeyword

  override def toJapaneseNotationString: String = TimeUp.kifKeyword

  override def toWesternNotationString: String = "Time Up"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${TimeUp.ki2Keyword}により${(!currentPlayer).toJapaneseNotationString()}の勝ち")
}

object TimeUp {
  val csaKeyword = "%TIME_UP"
  val kifKeyword = "切れ負け"
  val kifKeyword2 = "Time-up" // Used on 81Dojo
  val ki2Keyword = "時間切れ"
  val sfenExtendedKeyword = "t"
}

/**
  * Pause
  */
case object Pause extends SpecialMove {
  val csaKeyword = "%CHUDAN"
  val kifKeyword = "中断"
  val ki2Keyword: String = kifKeyword
  val sfenExtendedKeyword = "p"

  override def toCsaString: String = csaKeyword

  override def toKifString: String = kifKeyword

  override def toSfenExtendedString: String = sfenExtendedKeyword

  override def toJapaneseNotationString: String = kifKeyword

  override def toWesternNotationString: String = "Pause"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String = makeKi2String(numMoves, ki2Keyword)
}

// todo: impl KACHI, [+-]ILLEGAL_ACTION
