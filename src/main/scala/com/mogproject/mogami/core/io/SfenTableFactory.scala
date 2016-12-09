package com.mogproject.mogami.core.io

/**
  *
  */
trait SfenTableFactory[T <: SfenLike] extends SfenFactory[T] {

  val sfenTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](sfenTable)

  override def parseSfenString(s: String): Option[T] = tableFactory.get(s)(apply)

  def apply(id: Int): T

}
