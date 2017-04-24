package com.mogproject.mogami.core.io.sfen

/**
  * USEN -- Url Safe SFEN-Extended Notation
  */
trait UsenFactory[T <: UsenLike] {

  def parseUsenString(s: String): T

}
