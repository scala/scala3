package scala.runtime

import scala.collection.immutable.TreeMap

class EnumValues[E <: Enum] {
  private[this] var myMap: Map[Int, E] = TreeMap.empty
  private[this] var fromNameCache: Map[String, E] = null

  def register(v: E) = {
    require(!myMap.contains(v.ordinal))
    myMap = myMap.updated(v.ordinal, v)
    fromNameCache = null
  }

  def fromInt: Map[Int, E] = myMap
  def fromName: Map[String, E] = {
    // TODO remove cast when scala.Enum is bootstrapped
    if (fromNameCache == null) fromNameCache = myMap.values.map(v => v.asInstanceOf[Product].productPrefix -> v).toMap
    fromNameCache
  }
  def values: Iterable[E] = myMap.values
}
