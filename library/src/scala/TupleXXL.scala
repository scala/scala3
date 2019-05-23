package scala
import java.util.Arrays.{deepEquals, deepHashCode}

final class TupleXXL private (es: Array[Object]) extends Product {
  assert(es.length > 22)

  def productElement(n: Int): Any = es(n)
  def productArity: Int = es.length

  override def toString = elems.mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + deepHashCode(elems)
  override def canEqual(that: Any): Boolean = that match {
    case that: TupleXXL => that.productArity == this.productArity
    case _ => false
  }

  override def equals(that: Any) = that match {
    case that: TupleXXL => deepEquals(this.elems, that.elems)
    case _ => false
  }
  def elems: Array[Object] = es

  def tailXXL: TupleXXL = {
    assert(es.length > 23)
    new TupleXXL(es.tail)
  }

  def toArray: Array[Object] = es.clone
}
object TupleXXL {
  def fromIterator(elems: Iterator[Any]) = new TupleXXL(elems.map(_.asInstanceOf[Object]).toArray)
  def apply(elems: Array[Object]) = new TupleXXL(elems.clone)
  def apply(elems: Any*) = new TupleXXL(elems.asInstanceOf[Seq[Object]].toArray)
  def unapplySeq(x: TupleXXL): Option[Seq[Any]] = Some(x.elems.toSeq)
}
