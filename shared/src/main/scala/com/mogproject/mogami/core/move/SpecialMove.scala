package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.{Player, Square}
import com.mogproject.mogami.core.state.State

sealed trait SpecialMove extends CsaLike with KifLike with UsenLike {
  def toJapaneseNotationString: String

  def toWesternNotationString: String

  def toKi2String(currentPlayer: Player, numMoves: Int): String

  protected def makeKi2String(numMoves: Int, description: String): String = s"まで${numMoves}手で${description}"

  def getElapsedTime: Option[Int]

  def dropElapsedTime: SpecialMove
}

object SpecialMove {
  def parseUsenString(s: String, lastState: State, lastMoveTo: Option[Square]): SpecialMove = s.headOption match {
    case Some(Resign.usenKeyword) => Resign()
    case Some(TimeUp.usenKeyword) => TimeUp()
    case Some(Pause.usenKeyword) => Pause
    case Some(DeclareWin.usenKeyword) => DeclareWin()
    case Some(IllegalMove.usenKeyword) =>
      IllegalMove(MoveBuilderSfen.parseUsenString(s.drop(1)).toMove(lastState, lastMoveTo, isStrict = false).getOrElse(
        throw new RecordFormatException(1, s"invalid illegal move: ${s.drop(1)}")
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

  override def toUsenString: String = s"${IllegalMove.usenKeyword}${move.toUsenString}"

  override def toJapaneseNotationString: String = move.toJapaneseNotationString

  override def toWesternNotationString: String = move.toWesternNotationString

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の${IllegalMove.ki2Keyword}勝ち")

  override def getElapsedTime: Option[LineNo] = move.elapsedTime

  override def dropElapsedTime: SpecialMove = IllegalMove(move.copy(elapsedTime = None))
}

object IllegalMove {
  val csaKeyword = "%ILLEGAL_MOVE"
  val kifKeyword = "反則手"
  val ki2Keyword = "反則"
  val usenKeyword = 'i'
}

/**
  * Resign
  *
  * @param elapsedTime elapsed time
  */
case class Resign(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = Resign.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = Resign.kifKeyword + timeToKifString(elapsedTime)

  override def toUsenString: String = Resign.usenKeyword.toString

  override def toJapaneseNotationString: String = Resign.kifKeyword

  override def toWesternNotationString: String = "Resign"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${(!currentPlayer).toJapaneseNotationString()}の勝ち")

  override def getElapsedTime: Option[LineNo] = elapsedTime

  override def dropElapsedTime: SpecialMove = Resign(None)
}

object Resign {
  val csaKeyword = "%TORYO"
  val kifKeyword = "投了"
  val usenKeyword = 'r'
}

/**
  * Time up
  *
  * @param elapsedTime elapsed time
  */
case class TimeUp(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = TimeUp.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = TimeUp.kifKeyword + timeToKifString(elapsedTime)

  override def toUsenString: String = TimeUp.usenKeyword.toString

  override def toJapaneseNotationString: String = TimeUp.kifKeyword

  override def toWesternNotationString: String = "Time Up"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String =
    makeKi2String(numMoves, s"${TimeUp.ki2Keyword}により${(!currentPlayer).toJapaneseNotationString()}の勝ち")

  override def getElapsedTime: Option[LineNo] = elapsedTime

  override def dropElapsedTime: SpecialMove = TimeUp(None)
}

object TimeUp {
  val csaKeyword = "%TIME_UP"
  val kifKeyword = "切れ負け"
  val kifKeyword2 = "Time-up" // Used on 81Dojo
  val ki2Keyword = "時間切れ"
  val usenKeyword = 't'
}

/**
  * Pause
  */
case object Pause extends SpecialMove {
  val csaKeyword = "%CHUDAN"
  val kifKeyword = "中断"
  val ki2Keyword: String = kifKeyword
  val usenKeyword = 'p'

  override def toCsaString: String = csaKeyword

  override def toKifString: String = kifKeyword

  override def toUsenString: String = usenKeyword.toString

  override def toJapaneseNotationString: String = kifKeyword

  override def toWesternNotationString: String = "Pause"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String = makeKi2String(numMoves, ki2Keyword)

  override def getElapsedTime: Option[LineNo] = None

  override def dropElapsedTime: SpecialMove = this
}


/**
  * Declare win
  *
  * @param elapsedTime elapsed time
  */
case class DeclareWin(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = DeclareWin.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = DeclareWin.kifKeyword + timeToKifString(elapsedTime)

  override def toUsenString: String = DeclareWin.usenKeyword.toString

  override def toJapaneseNotationString: String = DeclareWin.kifKeyword

  override def toWesternNotationString: String = "Declare Win"

  override def toKi2String(currentPlayer: Player, numMoves: Int): String = makeKi2String(numMoves, DeclareWin.kifKeyword)

  override def getElapsedTime: Option[LineNo] = elapsedTime

  override def dropElapsedTime: SpecialMove = DeclareWin(None)
}

object DeclareWin {
  val csaKeyword = "%JISHOGI"
  val csaKeyword2 = "%KACHI"
  val kifKeyword = "持将棋"
  val usenKeyword = 'j'
}

// todo: impl [+-]ILLEGAL_ACTION
