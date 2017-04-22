package com.mogproject.mogami.core.io.csa

import com.mogproject.mogami.core.io.{BaseTableFactory, NonEmptyLines}

/**
  *
  */
trait CsaTableFactory[T <: CsaLike] extends CsaFactory[T] {

  val typeName: String

  val csaTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](typeName, csaTable)

  override def parseCsaString(nel: NonEmptyLines): T = tableFactory.parse(nel)(apply)

  def apply(id: Int): T

}
