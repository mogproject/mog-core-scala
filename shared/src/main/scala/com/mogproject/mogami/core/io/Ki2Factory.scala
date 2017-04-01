package com.mogproject.mogami.core.io

/**
  *
  */
trait Ki2Factory[T <: Ki2Like] {

  def parseKi2String(s: String): Option[T]

}
