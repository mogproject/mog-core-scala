package com.mogproject.mogami.util

/**
  * Utility functions for maps
  */
object MapUtil {
  /**
    * Merge two maps with a specific function.
    * @param a map1
    * @param b map2
    * @param mergeFunc binary function
    * @param default default value
    * @tparam K type of the map key
    * @tparam V type of the map value
    * @return merged map
    */
  def mergeMaps[K, V](a: Map[K, V], b: Map[K, V])(mergeFunc: (V, V) => V, default: V): Map[K, V] = {
    (a.keySet ++ b.keySet).map(k => k -> mergeFunc(a.getOrElse(k, default), b.getOrElse(k, default))).toMap
  }
}
