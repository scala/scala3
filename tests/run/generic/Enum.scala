package generic

import Shapes.Singleton
import scala.collection.mutable.ResizableArray
import scala.collection.immutable.Seq

trait Enum {
  def enumTag: Int
}

trait FiniteEnum extends Enum

class EnumValues[E <: Enum] extends ResizableArray[E] {
  private var valuesCache: Seq[E] = Nil
  def register(v: E) = {
    ensureSize(v.enumTag + 1)
    array(v.enumTag) = v
    valuesCache = null
  }
  def values: Seq[E] = {
    if (valuesCache == null) valuesCache = array.filter(_ != null).toList.asInstanceOf[scala.List[E]]
    valuesCache
  }
}
