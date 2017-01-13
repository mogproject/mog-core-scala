package com.mogproject.mogami.core.io

/**
  *
  */
trait KifFactory[T <: KifLike] {

  def parseKifString(s: String): Option[T]

}
