package scala
import annotation.showAsInfix
import compiletime._
import internal._

given TupleOps[T1 <: Tuple](t1: T1) {
  import tupleops._

  def zip[T2 <: Tuple, U <: Tuple](t2: T2) given (z: Zip.Aux[T1, T2, U]): U = z(t1, t2)
}

object tupleops {
  trait Zip[T1 <: Tuple, T2 <: Tuple] {
    type Res <: Tuple
    def apply(t1: T1, t2: T2): Res
  }

  object Zip {
    import Tuple.{ Head, Tail }

    type Aux[T1 <: Tuple, T2 <: Tuple, Res0] = Zip[T1, T2] { type Res = Res0 }

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
}