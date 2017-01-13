package com.mogproject.mogami.core.io

/**
  *
  */
trait KifTableFactory[T <: KifLike] extends KifFactory[T] {

  val kifTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](kifTable)

  override def parseKifString(s: String): Option[T] = tableFactory.get(s)(apply)

  def apply(id: Int): T

}
