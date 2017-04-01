package com.mogproject.mogami.core.io

/**
  *
  */
trait KifTableFactory[T <: KifLike] extends KifFactory[T] {

  val typeName: String

  val kifTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](typeName, kifTable)

  override def parseKifString(nel: NonEmptyLines): T = tableFactory.parse(nel)(apply)

  def apply(id: Int): T

}
