package com.mogproject.mogami.core.io

/**
  *
  */
trait IOFactoryLike {
  final protected[io] def toLines(s: String, normalizeString: Seq[String] => Lines): Lines = normalizeString(s.split("\n"))

  final protected[io] def toNonEmptyLines(lines: Lines): NonEmptyLines =
    if (lines.isEmpty) throw new RecordFormatException(0, "Empty input") else NonEmptyLines(lines)
}
