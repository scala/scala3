package scala

final class TupleXXL private (es: Array[Object]) {
  override def toString = elems.mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + elems.deep.hashCode
  override def equals(that: Any) = that match {
    case that: TupleXXL => this.elems.deep.equals(that.elems.deep)
    case _ => false
  }
  def elems: Array[Object] = es
}
object TupleXXL {
  def apply(elems: Array[Object]) = new TupleXXL(elems.clone)
}
