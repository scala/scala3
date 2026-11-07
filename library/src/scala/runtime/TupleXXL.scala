package scala.runtime

import language.experimental.captureChecking

/** The runtime representation of tuples with more than 22 elements.
 *
 *  Unlike `Tuple1` to `Tuple22`, which store their elements in individual
 *  fields, a `TupleXXL` stores all elements in a single immutable array.
 *  Instances are created by the factory methods on the companion object
 *  (the constructor is private and requires more than 22 elements) and arise
 *  from the operations in [[scala.runtime.Tuples]] and from compiler-generated
 *  code; user code manipulates them through the [[scala.Tuple]] API.
 */
final class TupleXXL private (es: IArray[Object]) extends Product {
  assert(es.length > 22)

  /** Returns the element of this tuple at index `n`.
   *
   *  @param n the 0-based index of the element
   *  @return the element at index `n`
   *  @throws IndexOutOfBoundsException if `n` is negative or not less than `productArity`
   */
  def productElement(n: Int): Any = es(n)
  /** Returns the number of elements of this tuple; always greater than 22. */
  def productArity: Int = es.length
  /** Returns the string `"Tuple"`, regardless of arity, unlike the `TupleN` classes whose prefix includes the arity. */
  override def productPrefix: String = "Tuple"

  // NOTE: For historical reasons, we cannot change this method
  // to `def toString(): String`. See #24461 for more informations.
  /** Returns the elements of this tuple, comma-separated and enclosed in parentheses, in the same format as the `TupleN` classes. */
  override def toString: String =
    elems.asInstanceOf[Array[Object]].mkString("(", ",", ")")

  // NOTE: For historical reasons, we cannot change this method
  // to `def hashCode(): Int`. See #24461 for more informations.
  /** Returns a hash code computed from the elements of this tuple, using the standard product hash of [[scala.runtime.ScalaRunTime]], so tuples that are `equals` have equal hash codes. */
  override def hashCode: Int =
    scala.runtime.ScalaRunTime._hashCode(this)

  /** Returns whether `that` can be compared for equality against this tuple.
   *
   *  @param that the value to test
   */
  override def canEqual(that: Any): Boolean = that match {
    case that: TupleXXL => that.productArity == this.productArity
    case _ => false
  }

  /** Returns whether `that` is a `TupleXXL` with the same elements as this tuple.
   *
   *  Two `TupleXXL`s are equal if they have the same arity and their elements
   *  are pairwise equal according to `==`. Two tuples sharing the same backing
   *  array compare equal without inspecting the elements. A value that is not
   *  a `TupleXXL` is never equal to this tuple.
   *
   *  @param that the value to compare against
   */
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

  /** Returns the immutable array backing this tuple; the array itself is returned, not a copy. */
  def elems: IArray[Object] = es

  /** Returns a new `TupleXXL` containing all elements of this tuple except the 1st.
   *
   *  This tuple must have at least 24 elements (asserted), so that the tail
   *  still has more than 22. The compiler emits calls to this method when
   *  optimizing `tail` on tuples statically known to be that large; the general
   *  case goes through [[scala.runtime.Tuples.tail]] instead.
   */
  def tailXXL: TupleXXL = {
    assert(es.length > 23)
    new TupleXXL(es.asInstanceOf[Array[Object]].tail.asInstanceOf[IArray[Object]]) // TODO use IArray.tail
  }

  /** Returns a fresh array containing the elements of this tuple; the backing array is cloned, so mutating the result does not affect this tuple. */
  def toArray: Array[Object] = es.asInstanceOf[Array[Object]].clone // TODO use IArray.toArray
}
object TupleXXL {
  /** Creates a `TupleXXL` containing the values produced by `elems`, in order.
   *
   *  The iterator is fully consumed into a fresh array.
   *
   *  @param elems the iterator supplying the elements; must produce more than 22 values (asserted)
   */
  def fromIterator(elems: Iterator[Any]): TupleXXL = new TupleXXL(elems.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
  /** Creates a `TupleXXL` backed directly by `elems`; the array is not copied.
   *
   *  @param elems the immutable array to use as the tuple's backing storage; must have more than 22 elements (asserted)
   */
  def fromIArray(elems: IArray[Object]): TupleXXL = new TupleXXL(elems)
  /** Creates a `TupleXXL` with the given elements.
   *
   *  @param elems the elements of the tuple; more than 22 must be supplied (asserted)
   */
  def apply(elems: Any*): TupleXXL = new TupleXXL(IArray(elems.asInstanceOf[Seq[AnyRef]]*))
  /** Extracts the elements of a `TupleXXL`, enabling `case TupleXXL(xs*)` patterns.
   *
   *  @param x the tuple to extract
   *  @return `Some` containing the elements of `x` in order; never `None`
   */
  def unapplySeq(x: TupleXXL): Option[Seq[Any]] = Some(x.elems.asInstanceOf[Array[Object]].toSeq) // TODO use IArray.toSeq
}
