package com.mogproject.mogami.core.game

import com.mogproject.mogami.core.io.csa.CsaLike

/**
  * Game information
  */
case class GameInfo(tags: Map[Symbol, String] = Map()) extends CsaLike {

  require(validateTagKeys)

  def validateTagKeys: Boolean = tags.keys forall { k => GameInfo.keys.map(_._1).contains(k) }

  def updated(key: Symbol, value: String): GameInfo = GameInfo(tags.updated(key, value))

  def toCsaString: String = {
    GameInfo.keys.toList.flatMap {
      case (k, c) if tags.contains(k) => List(c + tags(k))
      case _ => Nil
    } mkString "\n"
  }

  // todo: impl toKifString
  /*
  example:

#KIF version=2.0 encoding=UTF-8
開始日時：2017/03/13 ??:??
終了日時：2017/03/13 ??:??
場所：81Dojo (ver.2016/03/20)
持ち時間：15分+60秒
手合割：平手
先手：black
後手：white
   */
}

object GameInfo {
  /** pairs of a symbol name and its csa-formatted string */
  val keys: Seq[(Symbol, String)] = Seq(
    (Symbol("formatVersion"), "V"),
    (Symbol("blackName"), "N+"),
    (Symbol("whiteName"), "N-"),
    (Symbol("event"), "$EVENT:"),
    (Symbol("site"), "$SITE:"),
    (Symbol("start"), "$START:"),
    (Symbol("startTime"), "$START_TIME:"),
    (Symbol("endTime"), "$END_TIME:"),
    (Symbol("timeLimit"), "$TIME_LIMIT:"),
    (Symbol("opening"), "$OPENING:")
  )
}