package com.mogproject.mogami.core.io

/**
  *
  */
trait SfenFactory[T <: SfenLike] {
  val sfenTable: Seq[String]

  private lazy val invertedTable: Map[String, T] = sfenTable.zipWithIndex.map(x => x._1 -> apply(x._2)).toMap

  def apply(id: Int): T

  def parseSfenString(s: String): Option[T] = invertedTable.get(s)
}
