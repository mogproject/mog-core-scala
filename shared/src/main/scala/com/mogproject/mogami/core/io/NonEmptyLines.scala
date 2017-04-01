package com.mogproject.mogami.core.io

/**
  *
  */
case class NonEmptyLines(lines: Lines) {
  require(lines.nonEmpty, "lines must not be empty")
}

object NonEmptyLines {
  /**
    * create single-line sequence
    */
  def apply(lineNo: LineNo, s: String): NonEmptyLines = apply((s, lineNo))

  def apply(line: Line): NonEmptyLines = new NonEmptyLines(Seq(line))
}

