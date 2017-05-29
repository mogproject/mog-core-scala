package com.mogproject.mogami.core.io.kif

import com.mogproject.mogami.core.io.{IOFactoryLike, Lines, NonEmptyLines}
import com.mogproject.mogami.core.state.StateCache

/**
  *
  */
object Ki2Factory {
  def normalizeString(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if !ln.startsWith("*") && !ln.startsWith("#") // ignore comment lines
    if ln.nonEmpty
  } yield {
    (ln, n + 1)
  }
}

trait Ki2Factory[T <: Ki2Like] extends IOFactoryLike {

  final def parseKi2String(s: String): T = parseKi2String(toLines(s, Ki2Factory.normalizeString))

  final def parseKi2String(lines: Lines): T = parseKi2String(toNonEmptyLines(lines))

  def parseKi2String(lines: NonEmptyLines): T

}

trait Ki2GameFactory[T <: Ki2Like] extends IOFactoryLike {

  final def parseKi2String(s: String)(implicit stateCache: StateCache): T = parseKi2String(toLines(s, Ki2Factory.normalizeString))

  final def parseKi2String(lines: Lines)(implicit stateCache: StateCache): T = parseKi2String(toNonEmptyLines(lines))

  def parseKi2String(lines: NonEmptyLines)(implicit stateCache: StateCache): T

}
