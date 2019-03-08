package scala.reflect
import annotation.tailrec
import collection.mutable.ArrayBuffer

/** The part of `Generic` instances that is common for all instances of a class.
 *  @param runtimeClass The runtime class instance
 *  @param labelsStr    A string encoding all case and element labels according to the
 *                      following grammar:
 *
 *                      labelString   ::= caseString { caseSeparator caseString }
 *                      caseString    ::= elemString { elemSeparator elemString }
 *                      caseSeparator ::= '\u0001'
 *                      elemSeparator ::= '\u0000'
 *                      elemString: "any sequence of characters not containing '\u0000` or `\u0001`"
 */
class GenericClass(val runtimeClass: Class[_], labelsStr: String) {
  import GenericClass._

  /** A mirror of case with ordinal number `ordinal` and elements as given by `Product` */
  def mirror(ordinal: Int, product: Product): Mirror =
    new Mirror(this, ordinal, product)

  /** A mirror of a case with no elements */
  def mirror(ordinal: Int): Mirror =
    mirror(ordinal, EmptyProduct)

  /** A mirror with elements given as an array */
  def mirror(ordinal: Int, elems: Array[AnyRef]): Mirror =
    mirror(ordinal, new ArrayProduct(elems))

  /** A mirror with an initial empty array of `numElems` elements, to be filled in. */
  def mirror(ordinal: Int, numElems: Int): Mirror =
    mirror(ordinal, new Array[AnyRef](numElems))

  /** Case and element labels as a two-dimensional array.
   *  Each row of the array contains a case label, followed by the labels of the elements of that case.
   */
  val label: Array[Array[String]] =
    initLabels(0, 0, new ArrayBuffer[String], new ArrayBuffer[Array[String]])

  private def initLabels(start: Int, cur: Int,
                         elems: ArrayBuffer[String],
                         cases: ArrayBuffer[Array[String]]): Array[Array[String]] = {
    def addElem = elems += labelsStr.substring(start, cur)
    def addCase = cases += addElem.toArray
    if (cur == labelsStr.length)
      addCase.toArray
    else if (labelsStr(cur) == caseSeparator)
      initLabels(cur + 1, cur + 1, new ArrayBuffer, addCase)
    else if (labelsStr(cur) == elemSeparator)
      initLabels(cur + 1, cur + 1, addElem, cases)
    else
      initLabels(start, cur + 1, elems, cases)
  }
}

object GenericClass {
  private final val elemSeparator = '\u0000'
  private final val caseSeparator = '\u0001'

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