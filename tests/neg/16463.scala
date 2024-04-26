//> using scala "3.2.1"

import scala.compiletime.ops.int._

object TupleOps {
  import Tuple._

  type Reduce[T <: NonEmptyTuple, F[_, _]] =
    Fold[Tuple.Tail[T], Tuple.Head[T], F]

  type Maximum[T <: NonEmptyTuple] = Reduce[
    T,
    [A, B] =>> (A, B) match {
      case (Int, Int) => A `Max` B
    }
  ]

  type IndexOfRec[T <: Tuple, Elem, I <: Int] = Tuple.Elem[T, I] match {
    case Elem => I
    case _    => IndexOfRec[T, Elem, I + 1]
  }

  type IndexOf[T <: Tuple, Elem] = IndexOfRec[T, Elem, 0]

  type DropLargest[T <: NonEmptyTuple] =
    T `IndexOf` Maximum[T] match {
      case Int =>
        (
          (T `Take` (T `IndexOf` Maximum[T])) `Concat`
          (T `Drop` ((T `IndexOf` Maximum[T]) + 1))
        ) *: EmptyTuple
    }

  type BubbleSort[T <: Tuple] = T match {
    case EmptyTuple => EmptyTuple
    case NonEmptyTuple =>
      BubbleSort[DropLargest[T]] `Concat` (Maximum[T] *: EmptyTuple)
  }
}

object demo extends App {
  println(compiletime.constValue[TupleOps.BubbleSort[(1, 2)]]) // error: Recursion limit exceeded
}
