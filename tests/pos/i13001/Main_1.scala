case class Foo(a: String)

trait Arbitrary[T]
trait Gen[+T]

object ArbitraryDerivation:
  given deriveArb[A](using gen: DerivedGen[A]): Arbitrary[A] = ???

opaque type DerivedGen[A] = Gen[A]
object DerivedGen extends DerivedGenInstances

sealed abstract class DerivedGenInstances:
  inline given derived[A](using gen: K0.Generic[A]): DerivedGen[A] =
    val dummy: DerivedGen[A] = ???
    gen.derive(dummy, dummy)

// from shapeless3-deriving
import scala.deriving.*
object K0 {
  type Kind[C, O] = C { type Kind = K0.type ; type MirroredType = O ; type MirroredElemTypes <: Tuple }
  type Generic[O] = Kind[Mirror, O]
  type ProductGeneric[O] = Kind[Mirror.Product, O]
  type CoproductGeneric[O] = Kind[Mirror.Sum, O]

  extension [F[_], T](gen: Generic[T])
    inline def derive(f: => (ProductGeneric[T] & gen.type) ?=> F[T], g: => (CoproductGeneric[T] & gen.type) ?=> F[T]): F[T] =
      inline gen match {
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)
      }
}