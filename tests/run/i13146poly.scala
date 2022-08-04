import scala.deriving.*

trait Functor[F[_]]

object Functor:
  given [C]: Functor[[T] =>> C]()
  given Functor[[T] =>> Tuple1[T]]()
  given t2 [T]: Functor[[U] =>> (T, U)]()
  given t3 [T, U]: Functor[[V] =>> (T, U, V)]()

  def derived[F[_]](using m: Mirror { type MirroredType[X] = F[X] ; type MirroredElemTypes[_] }, r: Functor[m.MirroredElemTypes]): Functor[F] = new Functor[F] {}

case class Mono(i: Int) derives Functor
case class Poly[A](a: A) derives Functor
//case class Poly11[F[_]](fi: F[Int]) derives Functor
case class Poly2[A, B](a: A, b: B) derives Functor
case class Poly3[A, B, C](a: A, b: B, c: C) derives Functor

@main def Test = ()
