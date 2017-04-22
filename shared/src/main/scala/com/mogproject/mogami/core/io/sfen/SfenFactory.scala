package com.mogproject.mogami.core.io.sfen

/**
  *
  */
trait SfenFactory[T <: SfenLike] {

  def parseSfenString(s: String): T

}
