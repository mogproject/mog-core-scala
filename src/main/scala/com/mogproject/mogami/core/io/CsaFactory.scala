package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaFactory[T <: CsaLike] {

  def parseCsaString(s: String): Option[T]

}
