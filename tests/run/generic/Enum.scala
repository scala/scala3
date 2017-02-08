package generic

import Shapes.Singleton

trait Enum {
  def enumTag: Int
}

trait FiniteEnum extends Enum

abstract class EnumValues[E <: Enum](numVals: Int) {
  private var myValues = new Array[AnyRef](numVals)

  def registerEnumValue(v: E) =
    myValues(v.enumTag) = v

  def value: IndexedSeq[E] = (myValues: IndexedSeq[AnyRef]).asInstanceOf[IndexedSeq[E]]
}
