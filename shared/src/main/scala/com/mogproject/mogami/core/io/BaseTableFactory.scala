package com.mogproject.mogami.core.io

/**
  *
  */
case class BaseTableFactory[T](typeName: String, table: Seq[String]) {

  /**
    * @note "N/A" will be ignored.
    */
  private[this] lazy val invertedTable: Map[String, Int] =
    table.zipWithIndex.withFilter(_._1 != "N/A").map { case (s, i) => s -> i }.toMap

  private[this] def get(s: String)(constructor: Int => T): Option[T] = invertedTable.get(s).map(constructor)

  def parse(nel: NonEmptyLines)(constructor: Int => T): T = {
    if (nel.lines.length >= 2) {
      val (ln, n) = nel.lines(1)
      throw new RecordFormatException(n, s"too long ${typeName} expression: ${ln}")
    } else {
      val (ln, n) = nel.lines.head
      get(ln)(constructor).getOrElse(throw new RecordFormatException(n, s"invalid ${typeName}: ${ln}"))
    }
  }

}
