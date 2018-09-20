package scala
import java.util.Arrays.{deepEquals, deepHashCode}

final class TupleXXL private (es: Array[Object]) {
  assert(es.length > 22)
  override def toString = elems.mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + deepHashCode(elems)
  override def equals(that: Any) = that match {
    case that: TupleXXL => deepEquals(this.elems, that.elems)
    case _ => false
  }
  def elems: Array[Object] = es
}
object TupleXXL {
  def apply(elems: Array[Object]) = new TupleXXL(elems.clone)
  def apply(elems: Any*) = new TupleXXL(elems.asInstanceOf[Seq[Object]].toArray)
  def unapplySeq(x: TupleXXL): Option[Seq[Any]] = Some(x.elems.toSeq)
}
