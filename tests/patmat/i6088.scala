/** Natural transformation. */
trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

/** Higher-kinded pattern functor type class. */
trait HFunctor[H[f[_], i]] {
  def hmap[A[_], B[_]](nt: A ~> B): ([x] =>> H[A,x]) ~> ([x] =>> H[B,x])
}

/** Some HK pattern functor. */
enum ExprF[R[_],I] {
  case Const[R[_]](i: Int) extends ExprF[R,Int]
  case Neg[R[_]](l: R[Int]) extends ExprF[R,Int]
  case Eq[R[_]](l: R[Int], r: R[Int]) extends ExprF[R,Boolean]
}

/** Companion. */
object ExprF {
  given hfunctor: HFunctor[ExprF] {
    def hmap[A[_], B[_]](nt: A ~> B): ([x] =>> ExprF[A,x]) ~> ([x] =>> ExprF[B,x]) = {
      new ~>[[x] =>> ExprF[A,x], [x] =>> ExprF[B,x]] {
        def apply[I](fa: ExprF[A,I]): ExprF[B,I] = fa match {
          case Const(i) => Const(i)
          case Neg(l) => Neg(nt(l))
          case Eq(l, r) => Eq(nt(l), nt(r))
        }
      }
    }
  }
}
