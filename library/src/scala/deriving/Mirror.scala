package scala.deriving

// import language.experimental.captureChecking

/** Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents. */
sealed trait Mirror {

  /** The mirrored *-type. */
  type MirroredMonoType

  /** The name of the type. */
  type MirroredLabel <: String

  /** The names of the product elements. */
  type MirroredElemLabels <: Tuple
}

object Mirror {

  /** The `Mirror` for a sum type. */
  trait Sum extends Mirror { self =>
    /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal`.
     *
     *  @param x the instance of the sum type whose ordinal is returned
     */
    def ordinal(x: MirroredMonoType): Int
  }

  /** The `Mirror` for a product type. */
  trait Product extends Mirror { self =>

    /** Creates a new instance of type `T` with elements taken from product `p`.
     *
     *  @param p the product whose elements are used to create the new instance
     */
    def fromProduct(p: scala.Product): MirroredMonoType
  }

  trait Singleton extends Product {
    type MirroredMonoType = this.type
    type MirroredType = this.type
    type MirroredElemTypes = EmptyTuple
    type MirroredElemLabels = EmptyTuple
    def fromProduct(p: scala.Product): MirroredMonoType = this
  }

  /** A proxy for Scala 2 singletons, which do not inherit `Singleton` directly.
   *
   *  @param value the Scala 2 singleton instance being proxied
   */
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
    /** Creates a new instance of type `T` with elements taken from product `a`.
     *
     *  @tparam A the product type whose elements are used as input
     *  @tparam Elems the tuple type representing `A`'s elements, constrained to be a subtype of `T`'s `MirroredElemTypes`
     *  @param a the product instance whose elements are copied into the new `T`

     */
    def fromProductTyped[A <: scala.Product, Elems <: p.MirroredElemTypes](a: A)(using ProductOf[A] { type MirroredElemTypes = Elems }): T =
      p.fromProduct(a)

    /** Creates a new instance of type `T` with elements taken from tuple `t`.
     *
     *  @param t the tuple containing the elements for the new `T` instance
     */
    def fromTuple(t: p.MirroredElemTypes): T =
      p.fromProduct(t)
}
