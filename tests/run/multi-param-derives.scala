import scala.deriving._

object Test extends App {
  {
    trait Show[T]
    object Show {
      given as Show[Int] {}
      given [T] as Show[Tuple1[T]] given (st: Show[T]) {}
      given [T, U] as Show[(T, U)] given (st: Show[T], su: Show[U]) {}

      def derived[T] given (m: Mirror.Of[T], r: Show[m.MirroredElemTypes]): Show[T] = new Show[T] {}
    }

    case class Mono(i: Int) derives Show
    case class Poly[A](a: A) derives Show
    //case class Poly11[F[_]](fi: F[Int]) derives Show
    case class Poly2[A, B](a: A, b: B) derives Show
  }

  {
    trait Functor[F[_]]
    object Functor {
      def derived[F[_]] given (m: Mirror { type MirroredType = F }): Functor[F] = new Functor[F] {}
    }

    //case class Mono(i: Int) derives Functor
    case class Poly[A](a: A) derives Functor
    //case class Poly11[F[_]](fi: F[Int]) derives Functor
    //case class Poly2[A, B](a: A, b: B) derives Functor
  }

  {
    trait FunctorK[F[_[_]]]
    object FunctorK {
      def derived[F[_[_]]] given (m: Mirror { type MirroredType = F }): FunctorK[F] = new FunctorK[F] {}
    }

    //case class Mono(i: Int) derives FunctorK
    //case class Poly[A](a: A) derives FunctorK
    case class Poly11[F[_]](fi: F[Int]) derives FunctorK
    //case class Poly2[A, B](a: A, b: B) derives FunctorK
  }

  {
    trait Bifunctor[F[_, _]]
    object Bifunctor {
      def derived[F[_, _]] given (m: Mirror { type MirroredType = F }): Bifunctor[F] = ???
    }

    //case class Mono(i: Int) derives Bifunctor
    //case class Poly[A](a: A) derives Bifunctor
    //case class Poly11[F[_]](fi: F[Int]) derives Bifunctor
    case class Poly2[A, B](a: A, b: B) derives Bifunctor
  }
}
