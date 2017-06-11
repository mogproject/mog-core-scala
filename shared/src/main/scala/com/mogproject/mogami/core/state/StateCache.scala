package com.mogproject.mogami.core.state

import com.mogproject.mogami.core.state.StateHash.StateHash

import scala.collection.mutable

/**
  * Key-value storage of states
  */

class CacheNotFoundException(message: String) extends RuntimeException(message)


trait StateCache {
  def size: Int

  def set(state: State): StateHash

  def get(hash: StateHash): Option[State]

  def apply(hash: StateHash): State = get(hash).getOrElse(throw new CacheNotFoundException(f"failed to look up: hash=0x${hash}%016x"))

  def refresh(keep: Set[StateHash]): Unit

  def clear(): Unit

  /**
    * Number of keys
    */
  def numKeys: Int

  def hasKey(hash: StateHash): Boolean
}

/**
  * @note This map instance is NOT thread-safe. We don't use TrieMap due to the compatibility with Scala.js
  */
class ThreadUnsafeStateCache() extends StateCache {
  private[this] val partitionSize: Int = 1 << 4 // must be 2^n

  private[this] lazy val storage: Vector[mutable.Map[StateHash, State]] = Vector.fill(partitionSize)(mutable.Map.empty)

  override def size: Int = storage.size

  override def set(state: State): StateHash = {
    val m = storage(state.hash.toInt & (partitionSize - 1))
    if (!m.contains(state.hash)) m.update(state.hash, state)
    state.hash
  }

  override def get(hash: StateHash): Option[State] = storage(hash.toInt & (partitionSize - 1)).get(hash)

  override def refresh(keep: Set[StateHash]): Unit = storage.foreach { st =>
    (st.keySet -- keep).foreach(st.remove)
  }

  override def clear(): Unit = storage.foreach(_.clear())

  override def numKeys: Int = storage.map(_.size).sum

  override def hasKey(hash: StateHash): Boolean = storage(hash.toInt & (partitionSize - 1)).contains(hash)
}

object StateCache {

  object Implicits {

    implicit case object DefaultStateCache extends ThreadUnsafeStateCache

  }

  /**
    * Work with a temporary cache system
    */
  def withCache[A](execution: StateCache => A): A = {
    execution(new ThreadUnsafeStateCache())
  }
}
