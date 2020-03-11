object test {

  enum SUB[T, U] {
    case Refl[S]() extends SUB[S, S]
  }

  // injective(G) & f[S] <: G[S]  =X=> \forall t. f[t] <: Option[t]
  def foo[F[_]](fi: F[Int], sub: F[String] SUB Option[String]): Option[Int] =
    sub match {
      case SUB.Refl() =>
        fi // error
    }

  // injective(G) & f[x] <: G[S]  =X=> x <: S
  def bar[F[_], X](x: X, fi: F[Int], sub: F[X] SUB Option[Int]): Option[Int] =
    sub match {
      case SUB.Refl() =>
        val i: Int = x // error
        val y: X = (0: Int) // error
        fi // error
    }

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  // injective(G) & f <: G & f[x] <: G[T] =X=> x <: T
  def baz[F[_], X](x: X, ksub: F KSUB Option, sub: F[X] SUB Option[Int]) =
    ksub match {
      case KSUB.Refl() =>
        sub match {
          case SUB.Refl() =>
            val i: Int = x // error
            val y: X = (0: Int) // error
            ()
        }
    }

}
