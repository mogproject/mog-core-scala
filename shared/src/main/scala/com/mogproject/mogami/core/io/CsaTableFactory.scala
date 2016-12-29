package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaTableFactory[T <: CsaLike] extends CsaFactory[T] {

  val csaTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](csaTable)

  override def parseCsaString(s: String): Option[T] = tableFactory.get(s)(apply)

  def apply(id: Int): T

}
