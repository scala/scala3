package scala.runtime

final class TupleXXL private (es: IArray[Object]) extends Product {
  assert(es.length > 22)

  def productElement(n: Int): Any = es(n)
  def productArity: Int = es.length
  override def productPrefix: String = "Tuple"

  override def toString: String =
    elems.asInstanceOf[Array[Object]].mkString("(", ",", ")")

  override def hashCode: Int =
    scala.runtime.ScalaRunTime._hashCode(this)

  override def canEqual(that: Any): Boolean = that match {
    case that: TupleXXL => that.productArity == this.productArity
    case _ => false
  }

  override def equals(that: Any): Boolean = that match {
    case that: TupleXXL =>
      es.asInstanceOf[AnyRef].eq(that.elems.asInstanceOf[AnyRef]) || {
        if es.length != that.elems.length then return false
        var i = 0
        while i < es.length do
          if es(i) != that.elems(i) then return false
          i += 1
        true
      }
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
  def fromIterator(elems: Iterator[Any]): TupleXXL = new TupleXXL(elems.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
  def fromIArray(elems: IArray[Object]): TupleXXL = new TupleXXL(elems)
  def apply(elems: Any*): TupleXXL = new TupleXXL(IArray(elems.asInstanceOf[Seq[AnyRef]]: _*))
  def unapplySeq(x: TupleXXL): Option[Seq[Any]] = Some(x.elems.asInstanceOf[Array[Object]].toSeq) // TODO use IArray.toSeq
}
