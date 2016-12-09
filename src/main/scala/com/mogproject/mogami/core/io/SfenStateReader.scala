package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.State

/**
  * Reads Sfen-formatted state
  */
trait SfenStateReader {

  def parseSfenString(s: String): Option[State] = ???
}
