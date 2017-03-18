package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core._
import com.mogproject.mogami.util.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class KifStateReaderSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestKifStateReader extends KifStateReader

  // todo: implement
}
