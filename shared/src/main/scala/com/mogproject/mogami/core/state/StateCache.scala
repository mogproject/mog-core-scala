package com.mogproject.mogami.core.state

import com.mogproject.mogami.core.state.StateHash.StateHash

import scala.collection.mutable

/**
  * Key-value storage of states
  */
trait StateCache {
  def size: Int

  def set(state: State): Unit

  def get(hash: StateHash): Option[State]
}

/**
  * @note This map instance is NOT thread-safe. We don't use TrieMap due to the compatibility with Scala.js
  */
class ThreadUnsafeStateCache() extends StateCache {
  private[this] val storage: mutable.Map[StateHash, State] = mutable.Map.empty

  override def size: Int = storage.size

  override def set(state: State): Unit = if (!storage.contains(state.hash)) storage.update(state.hash, state)

  override def get(hash: StateHash): Option[State] = storage.get(hash)

}

object StateCache {

  object Implicits {

    implicit case object DefaultStateCache extends ThreadUnsafeStateCache

  }

}