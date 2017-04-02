package com.mogproject.mogami.core.io

/**
  *
  */
trait Ki2Factory[T <: Ki2Like] {

  final protected[io] def normalizeKi2String(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if !ln.startsWith("*") && !ln.startsWith("#") // ignore comment lines
    if ln.nonEmpty
  } yield {
    (ln, n + 1)
  }

  final def parseKi2String(s: String): T = parseKi2String(normalizeKi2String(s.split("\n")))

  final def parseKi2String(lines: Lines): T =
    if (lines.isEmpty) throw new RecordFormatException(0, "Empty input") else parseKi2String(NonEmptyLines(lines))

  def parseKi2String(lines: NonEmptyLines): T

}
