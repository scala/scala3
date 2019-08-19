package scala
import annotation.showAsInfix
import compiletime._
import internal._

given TupleOps[T1 <: Tuple](t1: T1) {
  import tupleops._

  def zip[T2 <: Tuple, U <: Tuple](t2: T2) given (z: Zip.Aux[T1, T2, U]): U = z(t1, t2)
  def map[F[_], R <: Tuple](f: [t] => t => F[t]) given (m: Map.Aux[T1, F, R]): R = m(t1, f)
}

object tupleops {
  import Tuple.{ Head, Tail }

  trait Zip[T1 <: Tuple, T2 <: Tuple] {
    type Res <: Tuple
    def apply(t1: T1, t2: T2): Res
  }

  object Zip {
    type Aux[T1 <: Tuple, T2 <: Tuple, Res0 <: Tuple] = Zip[T1, T2] { type Res = Res0 }

    given [T1 <: NonEmptyTuple, T2 <: NonEmptyTuple, TZ <: Tuple] as Aux[T1, T2, (Head[T1], Head[T2]) *: TZ]
    given (tz: Aux[Tail[T1], Tail[T2], TZ]) = new Zip[T1, T2] {
      type Res = (Head[T1], Head[T2]) *: TZ
      def apply(t1: T1, t2: T2): Res = (t1.head, t2.head) *: tz(t1.tail, t2.tail)
    }

    given as Aux[Unit, Unit, Unit] = new Zip[Unit, Unit] {
      type Res = Unit
      def apply(t1: Unit, t2: Unit): Unit = ()
    }
  }

  trait Map[T <: Tuple, F[_]] {
    type Res <: Tuple
    def apply(t: T, f: [t] => t => F[t]): Res
  }

  object Map {
    type Aux[T <: Tuple, F[_], Res0 <: Tuple] = Map[T, F] { type Res = Res0 }

    given [T <: NonEmptyTuple, F[_], MT <: Tuple] as Aux[T, F, F[Head[T]] *: MT]
    given (mt: Aux[Tail[T], F, MT]) = new Map[T, F] {
      type Res = F[Head[T]] *: MT
      def apply(t: T, f: [t] => t => F[t]): Res = f(t.head) *: mt(t.tail, f)
    }

    given [F[_]] as Aux[Unit, F, Unit] = new Map[Unit, F] {
      type Res = Unit
      def apply(t: Unit, f: [t] => t => F[t]): Unit = t
    }
  }
}
