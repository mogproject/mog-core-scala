package com.mogproject.mogami.core

/**
  *
  */
package object io {

  type LineNo = Int
  type Line = (String, LineNo)
  type Lines = Seq[Line]

  type CsaLike = com.mogproject.mogami.core.io.csa.CsaLike
  type CsaTableFactory[T <: CsaLike] = com.mogproject.mogami.core.io.csa.CsaTableFactory[T]
  type CsaFactory[T <: CsaLike] = com.mogproject.mogami.core.io.csa.CsaFactory[T]
  type CsaStateReader = com.mogproject.mogami.core.io.csa.CsaStateReader
  type CsaGameReader = com.mogproject.mogami.core.io.csa.CsaGameReader
  type CsaGameWriter = com.mogproject.mogami.core.io.csa.CsaGameWriter

  type KifLike = com.mogproject.mogami.core.io.kif.KifLike
  type KifTableFactory[T <: KifLike] = com.mogproject.mogami.core.io.kif.KifTableFactory[T]
  type KifFactory[T <: KifLike] = com.mogproject.mogami.core.io.kif.KifFactory[T]
  type KifStateReader = com.mogproject.mogami.core.io.kif.KifStateReader
  type KifGameReader = com.mogproject.mogami.core.io.kif.KifGameReader
  type KifGameWriter = com.mogproject.mogami.core.io.kif.KifGameWriter
  type KifBranchReader = com.mogproject.mogami.core.io.kif.KifBranchReader
  type KifBranchWriter = com.mogproject.mogami.core.io.kif.KifBranchWriter

  type Ki2Like = com.mogproject.mogami.core.io.kif.Ki2Like
  type Ki2Factory[T <: Ki2Like] = com.mogproject.mogami.core.io.kif.Ki2Factory[T]

  type SfenLike = com.mogproject.mogami.core.io.sfen.SfenLike
  type SfenTableFactory[T <: SfenLike] = com.mogproject.mogami.core.io.sfen.SfenTableFactory[T]
  type SfenFactory[T <: SfenLike] = com.mogproject.mogami.core.io.sfen.SfenFactory[T]
  type SfenStateReader = com.mogproject.mogami.core.io.sfen.SfenStateReader
  type SfenGameReader = com.mogproject.mogami.core.io.sfen.SfenGameReader
  type SfenGameWriter = com.mogproject.mogami.core.io.sfen.SfenGameWriter
  type SfenBranchReader = com.mogproject.mogami.core.io.sfen.SfenBranchReader
  type SfenBranchWriter = com.mogproject.mogami.core.io.sfen.SfenBranchWriter

  type UsenLike = com.mogproject.mogami.core.io.sfen.UsenLike
  type UsenFactory[T <: UsenLike] = com.mogproject.mogami.core.io.sfen.UsenFactory[T]
}
