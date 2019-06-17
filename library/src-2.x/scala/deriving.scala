/*
package scala

object deriving {

  /** Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.
   */
  sealed trait Mirror {

    /** The mirrored *-type */
    type MirroredMonoType

    /** The name of the type */
    type MirroredLabel <: String

    /** The names of the product elements */
    type MirroredElemLabels // <: Tuple // Bound removed to allow this to compile with
                                        // Scala 2. The full version of this file is in
                                        // library/src-3.x/scala/deriving.scala
  }

  object Mirror {

    /** The Mirror for a sum type */
    trait Sum extends Mirror { self =>
      /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal` */
      def ordinal(x: MirroredMonoType): Int
    }

    /** The Mirror for a product type */
    trait Product extends Mirror {

      /** Create a new instance of type `T` with elements taken from product `p`. */
      def fromProduct(p: scala.Product): MirroredMonoType
    }

    trait Singleton extends Product {
      type MirroredMonoType = this.type
      type MirroredType = this.type
      type MirroredElemTypes = Unit
      type MirroredElemLabels = Unit
      def fromProduct(p: scala.Product) = this
    }

    /** A proxy for Scala 2 singletons, which do not inherit `Singleton` directly */
    class SingletonProxy(val value: AnyRef) extends Product {
      type MirroredMonoType = value.type
      type MirroredType = value.type
      type MirroredElemTypes = Unit
      type MirroredElemLabels = Unit
      def fromProduct(p: scala.Product) = value
    }

    type Of[T]        = Mirror { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes <: Tuple }
    type ProductOf[T] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes <: Tuple }
    type SumOf[T]     = Mirror.Sum { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes <: Tuple }
  }

  /** Helper class to turn arrays into products */
  class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def this(size: Int) = this(new Array[AnyRef](size))
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  /** The empty product */
  object EmptyProduct extends ArrayProduct(Array[AnyRef]())

  /** Helper method to select a product element */
  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
}
*/
