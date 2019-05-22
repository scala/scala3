package scala

object deriving {

  /** Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.
   */
  sealed trait Mirror {

    /** The mirrored *-type */
    type MonoType

    /** The name of the type */
    type Label <: String
  }

  object Mirror {

    /** The Mirror for a sum type */
    trait Sum extends Mirror { self =>

      /** The types of the alternatives */
      type ElemTypes <: Tuple

      /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal` */
      def ordinal(x: MonoType): Int
    }

    /** The Mirror for a product type */
    trait Product extends Mirror {

      /** The types of the product elements */
      type ElemTypes <: Tuple

      /** The names of the product elements */
      type ElemLabels <: Tuple

      /** Create a new instance of type `T` with elements taken from product `p`. */
      def fromProduct(p: scala.Product): MonoType
    }

    trait Singleton extends Product {
      type MonoType = this.type
      type ElemTypes = Unit
      type ElemLabels = Unit

      def fromProduct(p: scala.Product) = this

      def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)
      def productArity: Int = 0
    }

    type Of[T]        = Mirror { type MonoType = T }
    type ProductOf[T] = Mirror.Product { type MonoType = T }
    type SumOf[T]     = Mirror.Sum { type MonoType = T }
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