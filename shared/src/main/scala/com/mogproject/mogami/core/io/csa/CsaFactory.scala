package com.mogproject.mogami.core.io.csa

import com.mogproject.mogami.core.io.{Lines, NonEmptyLines, RecordFormatException}

/**
  * Reads CSA-formatted string
  *
  * An exception can be thrown.
  */
trait CsaFactory[T <: CsaLike] {

  final protected[io] def normalizeCsaString(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if !ln.startsWith("'") // ignore comment lines
    chunk <- ln.split(',')
    if chunk.nonEmpty
  } yield {
    (chunk, n + 1)
  }

  final def parseCsaString(s: String): T = parseCsaString(normalizeCsaString(s.split("\n")))

  final def parseCsaString(lines: Lines): T =
    if (lines.isEmpty) throw new RecordFormatException(0, "Empty input") else parseCsaString(NonEmptyLines(lines))

  def parseCsaString(lines: NonEmptyLines): T

}
