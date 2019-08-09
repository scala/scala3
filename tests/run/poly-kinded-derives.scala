import scala.deriving._

object Test extends App {
  {
    trait Show[T]
    object Show {
      given as Show[Int] {}
      given [T] as Show[Tuple1[T]] given (st: Show[T]) {}
      given t2 [T, U] as Show[(T, U)] given (st: Show[T], su: Show[U]) {}
      given t3 [T, U, V] as Show[(T, U, V)] given (st: Show[T], su: Show[U], sv: Show[V]) {}

      def derived[T] given (m: Mirror.Of[T], r: Show[m.MirroredElemTypes]): Show[T] = new Show[T] {}
    }

    case class Mono(i: Int) derives Show
    case class Poly[A](a: A) derives Show
    //case class Poly11[F[_]](fi: F[Int]) derives Show
    case class Poly2[A, B](a: A, b: B) derives Show
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Show
  }

  {
    trait Functor[F[_]]
    object Functor {
      given [C] as Functor[[T] =>> C] {}
      given as Functor[[T] =>> Tuple1[T]] {}
      given t2 [T] as Functor[[U] =>> (T, U)] {}
      given t3 [T, U] as Functor[[V] =>> (T, U, V)] {}

      def derived[F[_]] given (m: Mirror { type MirroredType = F ; type MirroredElemTypes[_] }, r: Functor[m.MirroredElemTypes]): Functor[F] = new Functor[F] {}
    }

    case class Mono(i: Int) derives Functor
    case class Poly[A](a: A) derives Functor
    //case class Poly11[F[_]](fi: F[Int]) derives Functor
    case class Poly2[A, B](a: A, b: B) derives Functor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Functor
  }

  {
    trait FunctorK[F[_[_]]]
    object FunctorK {
      given [C] as FunctorK[[F[_]] =>> C] {}
      given [T] as FunctorK[[F[_]] =>> Tuple1[F[T]]]

      def derived[F[_[_]]] given (m: Mirror { type MirroredType = F ; type MirroredElemTypes[_[_]] }, r: FunctorK[m.MirroredElemTypes]): FunctorK[F] = new FunctorK[F] {}
    }

    case class Mono(i: Int) derives FunctorK
    //case class Poly[A](a: A) derives FunctorK
    case class Poly11[F[_]](fi: F[Int]) derives FunctorK
    //case class Poly2[A, B](a: A, b: B) derives FunctorK
    //case class Poly3[A, B, C](a: A, b: B, c: C) derives FunctorK
  }

  {
    trait Bifunctor[F[_, _]]
    object Bifunctor {
      given [C] as Bifunctor[[T, U] =>> C] {}
      given as Bifunctor[[T, U] =>> Tuple1[U]] {}
      given t2 as Bifunctor[[T, U] =>> (T, U)] {}
      given t3 [T] as Bifunctor[[U, V] =>> (T, U, V)] {}

      def derived[F[_, _]] given (m: Mirror { type MirroredType = F ; type MirroredElemTypes[_, _] }, r: Bifunctor[m.MirroredElemTypes]): Bifunctor[F] = ???
    }

    case class Mono(i: Int) derives Bifunctor
    case class Poly[A](a: A) derives Bifunctor
    //case class Poly11[F[_]](fi: F[Int]) derives Bifunctor
    case class Poly2[A, B](a: A, b: B) derives Bifunctor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Bifunctor
  }
}
