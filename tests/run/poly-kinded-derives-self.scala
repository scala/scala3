//> using options -language:experimental.modularity

// Self-based counterpart of run/poly-kinded-derives.scala, additionally checking
// that the derived instances can be summoned

import scala.deriving.*

object Test extends App {
  {
    trait Show:
      type Self
    object Show {
      private def instance[T]: T is Show = new Show { type Self = T }
      given Int is Show = instance
      given [T] => (st: T is Show) => Tuple1[T] is Show = instance
      given t2: [T, U] => (st: T is Show, su: U is Show) => (T, U) is Show = instance
      given t3: [T, U, V] => (st: T is Show, su: U is Show, sv: V is Show) => (T, U, V) is Show = instance

      def derived[T](using m: Mirror.Of[T], r: m.MirroredElemTypes is Show): T is Show = instance
    }

    case class Mono(i: Int) derives Show
    case class Poly[A](a: A) derives Show
    //case class Poly11[F[_]](fi: F[Int]) derives Show
    case class Poly2[A, B](a: A, b: B) derives Show
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Show

    assert(summon[Mono is Show] != null)
    assert(summon[Poly[Int] is Show] != null)
    assert(summon[Poly2[Int, Int] is Show] != null)
    assert(summon[Poly3[Int, Int, Int] is Show] != null)
  }

  {
    trait Functor:
      type Self[_]
    object Functor {
      private def instance[F[_]]: F is Functor = new Functor { type Self[X] = F[X] }
      given [C] => ([T] =>> C) is Functor = instance
      given ([T] =>> Tuple1[T]) is Functor = instance
      given t2: [T] => ([U] =>> (T, U)) is Functor = instance
      given t3: [T, U] => ([V] =>> (T, U, V)) is Functor = instance

      def derived[F[_]](using m: Mirror { type MirroredType[X] = F[X] ; type MirroredElemTypes[_] }, r: m.MirroredElemTypes is Functor): F is Functor = instance
    }

    case class Mono(i: Int) derives Functor
    case class Poly[A](a: A) derives Functor
    //case class Poly11[F[_]](fi: F[Int]) derives Functor
    case class Poly2[A, B](a: A, b: B) derives Functor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Functor

    assert(summon[([X] =>> Mono) is Functor] != null)
    assert(summon[Poly is Functor] != null)
    assert(summon[([X] =>> Poly2[Int, X]) is Functor] != null)
    assert(summon[([X] =>> Poly3[Int, Int, X]) is Functor] != null)
  }

  {
    trait FunctorK:
      type Self[_[_]]
    object FunctorK {
      private def instance[F[_[_]]]: F is FunctorK = new FunctorK { type Self[X[_]] = F[X] }
      given [C] => ([F[_]] =>> C) is FunctorK = instance
      given [T] => ([F[_]] =>> Tuple1[F[T]]) is FunctorK = instance

      def derived[F[_[_]]](using m: Mirror { type MirroredType[X[_]] = F[X] ; type MirroredElemTypes[_[_]] }, r: m.MirroredElemTypes is FunctorK): F is FunctorK = instance
    }

    case class Mono(i: Int) derives FunctorK
    //case class Poly[A](a: A) derives FunctorK
    case class Poly11[F[_]](fi: F[Int]) derives FunctorK

    assert(summon[([F[_]] =>> Mono) is FunctorK] != null)
    assert(summon[Poly11 is FunctorK] != null)
    //case class Poly2[A, B](a: A, b: B) derives FunctorK
    //case class Poly3[A, B, C](a: A, b: B, c: C) derives FunctorK
  }

  {
    trait Bifunctor:
      type Self[_, _]
    object Bifunctor {
      private def instance[F[_, _]]: F is Bifunctor = new Bifunctor { type Self[X, Y] = F[X, Y] }
      given [C] => ([T, U] =>> C) is Bifunctor = instance
      given ([T, U] =>> Tuple1[U]) is Bifunctor = instance
      given t2: ([T, U] =>> (T, U)) is Bifunctor = instance
      given t3: [T] => ([U, V] =>> (T, U, V)) is Bifunctor = instance

      def derived[F[_, _]](using m: Mirror { type MirroredType[X, Y] = F[X, Y] ; type MirroredElemTypes[_, _] }, r: m.MirroredElemTypes is Bifunctor): F is Bifunctor = ???
    }

    case class Mono(i: Int) derives Bifunctor
    case class Poly[A](a: A) derives Bifunctor
    //case class Poly11[F[_]](fi: F[Int]) derives Bifunctor
    case class Poly2[A, B](a: A, b: B) derives Bifunctor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Bifunctor

    // derived is ??? (as in the original), so only check that summoning typechecks
    def check =
      (summon[([X, Y] =>> Mono) is Bifunctor], summon[([X, Y] =>> Poly[Y]) is Bifunctor],
       summon[Poly2 is Bifunctor], summon[([X, Y] =>> Poly3[Int, X, Y]) is Bifunctor])
  }
}
