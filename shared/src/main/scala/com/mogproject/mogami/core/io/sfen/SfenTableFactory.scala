package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami.core.io.{BaseTableFactory, NonEmptyLines}

/**
  *
  */
trait SfenTableFactory[T <: SfenLike] extends SfenFactory[T] {

  val typeName: String

  val sfenTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](typeName, sfenTable)

  override def parseSfenString(s: String): T = tableFactory.parse(NonEmptyLines(1, s))(apply)

  def apply(id: Int): T

}
