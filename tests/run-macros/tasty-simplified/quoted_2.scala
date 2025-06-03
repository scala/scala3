//> using options -source:3.3

import Macros.simplified

object Test {

  def main(args: Array[String]): Unit = {
    type Const[C] = [T] =>> C
    type Id[T] = T
    case class Wrap[T](t: T)

    class Dummy
    type Apply[T[_]] = T[Dummy]
    type Unapply[F[_[_]], T] = T match {
      case Wrap[Apply[a]] => F[a]
      case Wrap[Dummy] => F[Id]
      case Wrap[c] => F[Const[c]]
    }

    type LiftP[F[_[_]], T[_]] = LiftP0[F, Apply[T]]

    type LiftP0[F[_[_]], T] <: Tuple = T match {
      case EmptyTuple => EmptyTuple
      case (a *:  b) => Unapply[F, Wrap[a]] *: LiftP0[F, b]
    }

    trait Functor[F[_]]

    type T1[t] = (List[t], Int, t, Option[t])

    val elems = simplified[LiftP[Functor, T1]]
    elems.foreach(println)
  }
}
