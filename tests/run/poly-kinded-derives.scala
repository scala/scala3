import scala.deriving.*

object Test extends App {
  {
    trait Show[T]
    object Show {
      given Show[Int]()
      given [T](using st: Show[T]): Show[Tuple1[T]]()
      given t2[T, U](using st: Show[T], su: Show[U]): Show[(T, U)]()
      given t3 [T, U, V](using st: Show[T], su: Show[U], sv: Show[V]): Show[(T, U, V)]()

      def derived[T](using m: Mirror.Of[T], r: Show[m.MirroredElemTypes]): Show[T] = new Show[T] {}
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
      given [C] => Functor[[T] =>> C]()
      given Functor[[T] =>> Tuple1[T]]()
      given t2: [T] => Functor[[U] =>> (T, U)]()
      given t3: [T, U] => Functor[[V] =>> (T, U, V)]()

      def derived[F[_]](using m: Mirror { type MirroredType[X] = F[X] ; type MirroredElemTypes[_] }, r: Functor[m.MirroredElemTypes]): Functor[F] = new Functor[F] {}
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
      given [C] => FunctorK[[F[_]] =>> C]()
      given [T] => FunctorK[[F[_]] =>> Tuple1[F[T]]]()

      def derived[F[_[_]]](using m: Mirror { type MirroredType[X[_]] = F[X] ; type MirroredElemTypes[_[_]] }, r: FunctorK[m.MirroredElemTypes]): FunctorK[F] = new FunctorK[F] {}
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
      given [C] => Bifunctor[[T, U] =>> C]()
      given Bifunctor[[T, U] =>> Tuple1[U]]()
      given t2: Bifunctor[[T, U] =>> (T, U)]()
      given t3: [T] => Bifunctor[[U, V] =>> (T, U, V)]()

      def derived[F[_, _]](using m: Mirror { type MirroredType[X, Y] = F[X, Y] ; type MirroredElemTypes[_, _] }, r: Bifunctor[m.MirroredElemTypes]): Bifunctor[F] = ???
    }

    case class Mono(i: Int) derives Bifunctor
    case class Poly[A](a: A) derives Bifunctor
    //case class Poly11[F[_]](fi: F[Int]) derives Bifunctor
    case class Poly2[A, B](a: A, b: B) derives Bifunctor
    case class Poly3[A, B, C](a: A, b: B, c: C) derives Bifunctor
  }
}
