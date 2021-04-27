import annotation.showAsInfix
import scala.compiletime._
import scala.compiletime.ops.int._

class TupleKOps {


  def tupleMerge(tuple1: TupleK, tuple2: TupleK): TupleK = ???

  def tupleMergeSort(tuple: TupleK): TupleK =
    if (tuple.size <= 1) tuple
    else {
      val (tuple1, tuple2) = tuple.splitAt(tuple.size / 2)
      // val (tuple1: TupleK, tuple2: TupleK) = tuple.splitAt(tuple.size / 2) // ok
      val (sorted1, sorted2) = (tupleMergeSort(tuple1), tupleMergeSort(tuple2))
      tupleMerge(sorted1, sorted2)
    }

}

sealed trait TupleK {
  import TupleK._
  /*inline*/ def size[This >: this.type <: TupleK]: Size[This] = ???
  /*inline*/ def splitAt[This >: this.type <: TupleK](n: Int): Split[This, n.type] = ???
}

object TupleK {
  type Size[X <: TupleK] <: Int = X match {
    case EmptyTupleK => 0
    case x #: xs => S[Size[xs]]
  }
  type Take[T <: TupleK, N <: Int] <: TupleK = N match {
    case 0 => EmptyTupleK
    case S[n1] => T match {
      case EmptyTupleK => EmptyTupleK
      case x #: xs => x #: Take[xs, n1]
    }
  }
  type Drop[T <: TupleK, N <: Int] <: TupleK = N match {
    case 0 => T
    case S[n1] => T match {
      case EmptyTupleK => EmptyTupleK
      case x #: xs => Drop[xs, n1]
    }
  }
  type Split[T <: TupleK, N <: Int] = (Take[T, N], Drop[T, N])
}

type EmptyTupleK = EmptyTupleK.type

object EmptyTupleK extends TupleK

sealed trait NonEmptyTupleK extends TupleK
sealed abstract class #:[+H, +T <: TupleK] extends NonEmptyTupleK
