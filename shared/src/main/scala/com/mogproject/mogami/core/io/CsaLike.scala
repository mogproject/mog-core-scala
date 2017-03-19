package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaLike {

  def toCsaString: String

  protected def timeToCsaString(time: Option[Int]): String = time.map(",T" + _.toString).getOrElse("")
}
