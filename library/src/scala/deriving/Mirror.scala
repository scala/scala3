package scala.deriving

import java.util.NoSuchElementException
import scala.annotation.experimental

/** Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.
 */
sealed trait Mirror {

  /** The mirrored *-type */
  type MirroredMonoType

  /** The name of the type */
  type MirroredLabel <: String

  /** The names of the product elements */
  type MirroredElemLabels <: Tuple
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

    /** Whether each product element has a default value */
    @experimental type MirroredElemHasDefaults <: Tuple

    /** The default argument of the product argument at given `index` */
    @experimental def defaultArgument(index: Int): Any =
      throw NoSuchElementException(String.valueOf(index))

  }

  trait Singleton extends Product {
    type MirroredMonoType = this.type
    type MirroredType = this.type
    type MirroredElemTypes = EmptyTuple
    type MirroredElemLabels = EmptyTuple
    def fromProduct(p: scala.Product): MirroredMonoType = this
  }

  /** A proxy for Scala 2 singletons, which do not inherit `Singleton` directly */
  class SingletonProxy(val value: AnyRef) extends Product {
    type MirroredMonoType = value.type
    type MirroredType = value.type
    type MirroredElemTypes = EmptyTuple
    type MirroredElemLabels = EmptyTuple
    def fromProduct(p: scala.Product): MirroredMonoType = value
  }

  type Of[T] = Mirror { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes <: Tuple }
  type ProductOf[T] = Mirror.Product { type MirroredType = T; type MirroredMonoType = T ; type MirroredElemTypes <: Tuple }
  type SumOf[T] = Mirror.Sum { type MirroredType = T; type MirroredMonoType = T; type MirroredElemTypes <: Tuple }

  extension [T](p: ProductOf[T])
    /** Create a new instance of type `T` with elements taken from product `a`. */
    def fromProductTyped[A <: scala.Product, Elems <: p.MirroredElemTypes](a: A)(using m: ProductOf[A] { type MirroredElemTypes = Elems }): T =
      p.fromProduct(a)

    /** Create a new instance of type `T` with elements taken from tuple `t`. */
    def fromTuple(t: p.MirroredElemTypes): T =
      p.fromProduct(t)
}
