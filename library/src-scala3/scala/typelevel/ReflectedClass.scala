package scala.typelevel
import annotation.tailrec

/** @param caseLabels The case and element labels of the described ADT as encoded strings.
*/
class ReflectedClass(caseLabels: Array[String]) {
  import ReflectedClass._

  /** A mirror of case with ordinal number `ordinal` and elements as given by `Product` */
  def mirror(ordinal: Int, product: Product): Mirror =
    new Mirror(this, ordinal, product)

  /** A mirror with elements given as an array */
  def mirror(ordinal: Int, elems: Array[AnyRef]): Mirror =
    mirror(ordinal, new ArrayProduct(elems))

  /** A mirror with an initial empty array of `numElems` elements, to be filled in. */
  def mirror(ordinal: Int, numElems: Int): Mirror =
    mirror(ordinal, new Array[AnyRef](numElems))

  /** A mirror of a case with no elements */
  def mirror(ordinal: Int): Mirror =
    mirror(ordinal, EmptyProduct)

  private[typelevel] def label(ordinal: Int, idx: Int): String = {
    val labels = caseLabels(ordinal)
    @tailrec def separatorPos(from: Int): Int =
      if (from == labels.length || labels(from) == separator) from
      else separatorPos(from + 1)
    @tailrec def findLabel(count: Int, idx: Int): String =
      if (idx == labels.length) ""
      else if (count == 0) labels.substring(idx, separatorPos(idx))
      else findLabel(if (labels(idx) == separator) count - 1 else count, idx + 1)
    findLabel(idx, 0)
  }
}

object ReflectedClass {

  private final val separator = '\000'

  /** Helper class to turn arrays into products */
  private class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  /** Helper object */
  private object EmptyProduct extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = throw new IndexOutOfBoundsException
    def productArity = 0
  }
}