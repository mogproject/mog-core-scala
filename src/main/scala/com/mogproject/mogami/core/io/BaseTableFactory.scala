package com.mogproject.mogami.core.io

/**
  *
  */
case class BaseTableFactory[T](table: Seq[String]) {

  private[this] lazy val invertedTable: Map[String, Int] =
    table.zipWithIndex.withFilter(_._1.nonEmpty).map { case (s, i) => s -> i }.toMap

  def get(s: String)(constructor: Int => T): Option[T] = invertedTable.get(s).map(constructor)

}
