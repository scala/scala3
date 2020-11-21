package scala.deriving

/** Helper class to turn arrays into products */
class ArrayProduct(val elems: Array[AnyRef]) extends Product {
  def this(size: Int) = this(new Array[AnyRef](size))
  def canEqual(that: Any): Boolean = true
  def productElement(n: Int): Any = elems(n)
  def productArity: Int = elems.length
  override def productIterator: Iterator[Any] = elems.iterator
  def update(n: Int, x: Any): Unit = elems(n) = x.asInstanceOf[AnyRef]
}

/** The empty product */
object EmptyProduct extends ArrayProduct(Array.emptyObjectArray)

/** Helper method to select a product element */
def productElement[T](x: Any, idx: Int): T =
  x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
