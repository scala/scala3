package scala.runtime

import scala.collection.immutable.Map

class EnumValues[E <: Enum] {
  private[this] var myMap: Map[Int, E] = Map()
  private[this] var fromNameCache: Map[String, E] = null

  def register(v: E) = {
    require(!myMap.contains(v.enumTag))
    myMap = myMap.updated(v.enumTag, v)
    fromNameCache = null
  }

  def fromInt: Map[Int, E] = myMap
  def fromName: Map[String, E] = {
    if (fromNameCache == null) fromNameCache = myMap.values.map(v => v.toString -> v).toMap
    fromNameCache
  }
  def values: Iterable[E] = myMap.values
}
