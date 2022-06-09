import scala.compiletime.ops.int.*

final class Label (val getLabel: String)

trait ShapelessPolyfill {

  type Represented[R] = R match {
    case IndexedSeq[a] => a
  }

  type TupleSized[R, A, N <: Int] <: Tuple = N match {
    case 0 => EmptyTuple
    case S[n] => A *: TupleSized[R, A, n]
  }

  extension [R, A, N <: Int] (s: TupleSized[R, A, N]) {
    def unsized: IndexedSeq[A] = s.productIterator.toIndexedSeq.asInstanceOf[IndexedSeq[A]]
  }

  type Nat = Int

  type Sized[Repr, L <: Nat] = TupleSized[Repr, Represented[Repr], L]

  object Sized {
    def apply[A](a1: A): Sized[IndexedSeq[A], 1] = Tuple1(a1)
  }
}
object poly extends ShapelessPolyfill
