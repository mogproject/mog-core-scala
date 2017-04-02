package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.Player
import com.mogproject.mogami.core.io.{CsaLike, KifLike}

sealed trait SpecialMove extends CsaLike with KifLike {
  def toJapaneseNotationString: String

  def toWesternNotationString: String

  def toKi2String(currentPlayer: Player, numMoves: Int): String

  protected def makeKi2String(numMoves: Int, description: String): String = s"まで${numMoves}手で${description}"
}

/**
  * Illegal move
  *
  * @param move illegal move
  */
case class IllegalMove(move: Move) extends SpecialMove {
  override def toCsaString: String = IllegalMove.csaKeyword

  override def toKifString: String = IllegalMove.kifKeyword

  override def toJapaneseNotationString: String = IllegalMove.kifKeyword

  override def toWesternNotationString: String = "Illegal Move"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の${IllegalMove.ki2Keyword}勝ち")
}

object IllegalMove {
  val csaKeyword = "%ILLEGAL_MOVE"
  val kifKeyword = "反則手"
  val ki2Keyword = "反則"
}

/**
  * Resign
  *
  * @param elapsedTime elapsed time
  */
case class Resign(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = Resign.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = Resign.kifKeyword + timeToKifString(elapsedTime)

  override def toJapaneseNotationString: String = Resign.kifKeyword

  override def toWesternNotationString: String = "Resign"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の勝ち")
}

object Resign {
  val csaKeyword = "%TORYO"
  val kifKeyword = "投了"
}

/**
  * Time up
  *
  * @param elapsedTime elapsed time
  */
case class TimeUp(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = TimeUp.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = TimeUp.kifKeyword + timeToKifString(elapsedTime)

  override def toJapaneseNotationString: String = TimeUp.kifKeyword

  override def toWesternNotationString: String = "Time Up"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${TimeUp.ki2Keyword}により${(!currentPlayer).toJapaneseNotationString()}の勝ち")
}

object TimeUp {
  val csaKeyword = "%TIME_UP"
  val kifKeyword = "切れ負け"
  val ki2Keyword = "時間切れ"
}

/**
  * Pause
  */
case object Pause extends SpecialMove {
  val csaKeyword = "%CHUDAN"
  val kifKeyword = "中断"
  val ki2Keyword = kifKeyword

  override def toCsaString: String = csaKeyword

  override def toKifString: String = kifKeyword

  override def toJapaneseNotationString: String = kifKeyword

  override def toWesternNotationString: String = "Pause"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String = makeKi2String(numMoves, ki2Keyword)
}

// todo: impl KACHI, [+-]ILLEGAL_ACTION
