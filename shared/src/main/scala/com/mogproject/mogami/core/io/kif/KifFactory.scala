package com.mogproject.mogami.core.io.kif

import com.mogproject.mogami.core.io.{Lines, NonEmptyLines, RecordFormatException}

/**
  *
  */
trait KifFactory[T <: KifLike] {

  /**
    * @note do not ignore comments (* or #) here
    * @param s
    * @return
    */
  final protected[io] def normalizeKifString(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if ln.nonEmpty
  } yield {
    (ln, n + 1)
  }

  final def parseKifString(s: String): T = parseKifString(normalizeKifString(s.split("\n")))

  final def parseKifString(lines: Lines): T =
    if (lines.isEmpty) throw new RecordFormatException(0, "Empty input") else parseKifString(NonEmptyLines(lines))

  def parseKifString(lines: NonEmptyLines): T

}
