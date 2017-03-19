package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.io.{CsaLike, KifLike}

sealed trait SpecialMove extends CsaLike with KifLike

case class IllegalMove(move: Move) extends SpecialMove {
  override def toCsaString: String = move.toCsaString + "\n" + IllegalMove.csaKeyword

  override def toKifString: String = move.toKifString + "\n" + IllegalMove.kifKeyword
}

object IllegalMove {
  val csaKeyword = "%ILLEGAL_MOVE"
  val kifKeyword = "反則手"
}

case class Resign(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = Resign.csaKeyword + timeToCsaString(elapsedTime)

  override def toKifString: String = Resign.kifKeyword + timeToKifString(elapsedTime)
}

object Resign {
  val csaKeyword = "%TORYO"
  val kifKeyword = "投了"
}

case class TimeUp(elapsedTime: Option[Int] = None) extends SpecialMove {
  override def toCsaString: String = TimeUp.csaKeyword

  override def toKifString: String = TimeUp.kifKeyword + timeToKifString(elapsedTime)
}

object TimeUp {
  val csaKeyword = "%TIME_UP"
  val kifKeyword = "切れ負け"
}

// todo: impl KACHI, [+-]ILLEGAL_ACTION
