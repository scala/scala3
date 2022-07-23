import scala.compiletime.ops.int.S

type sized[T, N <: Int] = T & { type Size = N }

abstract class TupOf[T, +A] {
  type Size <: Int
}

object TupOf {
  given Empty[A]: TupOf[EmptyTuple, A] with {
    type Size = 0
  }

  final given Cons[A, T <: Tuple, N <: Int](using p: T TupOf A sized N): TupOf[A *: T, A] with {
    val p0: T TupOf A sized N = p
    type Size = S[N]
  }
}

enum Vec[N <: Int, +A]:
  case VecNil extends Vec[0, Nothing]
  case VecCons[N0 <: Int, A](head: A, tail: Vec[N0, A]) extends Vec[S[N0], A]

object Vec {
  import TupOf._
  def apply[A, T <: Tuple](xs: T)(using p: T TupOf A): Vec[p.Size, A] = p match {
    case _: TupOf.Empty[A] => VecNil
    case p1: TupOf.Cons[a, t, n] =>
      VecCons(xs.head, apply(xs.tail)(using p1.p0))
  }

  def main(): Unit = {
    val vec1: Vec[3, Int] = VecCons(1, VecCons(2, VecCons(3, VecNil)))
    val vec2: Vec[3, Int] = Vec(1, 2, 3)
  }
}

