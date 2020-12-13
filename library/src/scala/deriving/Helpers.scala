package scala.deriving

/** Helper class to turn arrays into products */
@deprecated("explicitly create a `new Product {...}` wrapper for the array or use `Tuple.fromArray`", "3.0.0-M2")
class ArrayProduct(val elems: Array[AnyRef]) extends Product {
  def this(size: Int) = this(new Array[AnyRef](size))
  def canEqual(that: Any): Boolean = true
  def productElement(n: Int): Any = elems(n)
  def productArity: Int = elems.length
  override def productIterator: Iterator[Any] = elems.iterator
  def update(n: Int, x: Any): Unit = elems(n) = x.asInstanceOf[AnyRef]
}

/** The empty product */
@deprecated("use EmptyTuple instead", "3.0.0-M2")
object EmptyProduct extends ArrayProduct(Array.emptyObjectArray)

/** Helper method to select a product element */
@deprecated("use x.asInstanceOf[Product].productElement(idx).asInstanceOf[T] instead", "3.0.0-M2")
def productElement[T](x: Any, idx: Int): T =
  x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
