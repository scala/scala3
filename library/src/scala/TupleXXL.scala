package scala
import java.util.Arrays.{deepEquals, deepHashCode}

final class TupleXXL private (es: IArray[Object]) extends Product {
  assert(es.length > 22)

  def productElement(n: Int): Any = es(n)
  def productArity: Int = es.length

  override def toString = elems.asInstanceOf[Array[Object]].mkString("(", ",", ")")
  override def hashCode = getClass.hashCode * 41 + deepHashCode(elems.asInstanceOf[Array[Object]])
  override def canEqual(that: Any): Boolean = that match {
    case that: TupleXXL => that.productArity == this.productArity
    case _ => false
  }

  override def equals(that: Any) = that match {
    case that: TupleXXL => deepEquals(this.elems.asInstanceOf[Array[Object]], that.elems.asInstanceOf[Array[Object]])
    case _ => false
  }
  def elems: IArray[Object] = es

  def tailXXL: TupleXXL = {
    assert(es.length > 23)
    new TupleXXL(es.asInstanceOf[Array[Object]].tail.asInstanceOf[IArray[Object]]) // TODO use IArray.tail
  }

  def toArray: Array[Object] = es.asInstanceOf[Array[Object]].clone // TODO use IArray.toArray
}
object TupleXXL {
  def fromIterator(elems: Iterator[Any]) = new TupleXXL(elems.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
  def fromIArray(elems: IArray[Object]) = new TupleXXL(elems)
  def apply(elems: Any*) = new TupleXXL(IArray(elems.asInstanceOf[Seq[AnyRef]]: _*))
  def unapplySeq(x: TupleXXL): Option[Seq[Any]] = Some(x.elems.asInstanceOf[Array[Object]].toSeq) // TODO use IArray.toSeq
}
