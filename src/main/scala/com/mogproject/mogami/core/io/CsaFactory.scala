package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaFactory[T <: CsaLike] {

  def parseCsaString(s: String): Option[T]

  def parseCsaString(s: Seq[String]): Option[T] = parseCsaString(s.mkString("\n"))
}
