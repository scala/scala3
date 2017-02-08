package scala.runtime

import scala.collection.immutable.Seq
import scala.collection.mutable.ResizableArray

class EnumValues[E <: Enum] extends ResizableArray[E] {
  private var valuesCache: List[E] = Nil
  def register(v: E) = {
    ensureSize(v.enumTag + 1)
    size0 = size0 max (v.enumTag + 1)
    array(v.enumTag) = v
    valuesCache = null
  }
  def values: Seq[E] = {
    if (valuesCache == null) valuesCache = array.filter(_ != null).toList.asInstanceOf[scala.List[E]]
    valuesCache
  }
}
