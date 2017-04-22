package com.mogproject.mogami.core.io.kif

/**
  *
  */
trait KifLike {

  def toKifString: String

  protected def timeToKifString(time: Option[Int]): String = time.map(t => f" (${t / 60}%02d:${t % 60}%02d/)").getOrElse("")
}
