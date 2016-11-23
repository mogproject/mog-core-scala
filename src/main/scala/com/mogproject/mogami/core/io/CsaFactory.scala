package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaFactory[T <: CsaLike] {
  val csaTable: Seq[String]

  private lazy val invertedTable: Map[String, T] = csaTable.zipWithIndex.map(x => x._1 -> apply(x._2)).toMap

  def apply(id: Int): T

  def parseCsaString(s: String): Option[T] = invertedTable.get(s)

  def parseCsaString(s: Seq[String]): Option[T] = parseCsaString(s.mkString("\n"))
}
