package com.mogproject.mogami.core.io

/**
  *
  */
trait SfenFactory[T <: SfenLike] {

  def parseSfenString(s: String): Option[T]

}
