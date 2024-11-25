package scala.compiletime
import reflect.ClassTag

// This is in compiletime since it is an inline type class. Another
// possibility would be to put it in `scala.collection`

/** A typeclass that supports creating collection-like data from
 *  collection literals `[x1,...,xN]`.
 */
trait ExpressibleAsCollectionLiteral[+Coll]:

  /** The element type of the created collection */
  type Elem

  /** The inline method that creates the collection */
  inline def fromLiteral(inline xs: Elem*): Coll

object ExpressibleAsCollectionLiteral:

  // Some instances for standard collections. It would be good to have a method
  // that works for all collections in stdlib. But to do that I believe we either
  // have to put a given instance in Factory in stdlib, or write some macro
  // method here. I have not found a straightforward way to build a collection
  // of type `C` is all we know is the type.

  given seqFromLiteral: [T] => ExpressibleAsCollectionLiteral[collection.Seq[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*): Seq[T] = Seq(xs*)

  given mutableSeqFromLiteral: [T] => ExpressibleAsCollectionLiteral[collection.mutable.Seq[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = collection.mutable.Seq(xs*)

  given immutableSeqFromLiteral: [T] => ExpressibleAsCollectionLiteral[collection.immutable.Seq[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = collection.immutable.Seq(xs*)

  given vectorFromLiteral: [T] => ExpressibleAsCollectionLiteral[Vector[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = Vector[Elem](xs*)

  given listFromLiteral: [T] => ExpressibleAsCollectionLiteral[List[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = List(xs*)

  given arrayFromLiteral: [T: ClassTag] => ExpressibleAsCollectionLiteral[Array[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = Array(xs*)

  given iarrayFromLiteral: [T: ClassTag] => ExpressibleAsCollectionLiteral[IArray[T]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*) = IArray(xs*)

  given bitsetFromLiteral: ExpressibleAsCollectionLiteral[collection.immutable.BitSet]:
    type Elem = Int
    inline def fromLiteral(inline xs: Int*) = collection.immutable.BitSet(xs*)
