object test {

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  trait Mkr[F[_]] {
    def mk[T](t: T): F[T]
  }

  def foo[F[_]](mkr: Mkr[F], sub: F KSUB Option): Option[Int] =
    sub match {
      case KSUB.Refl() =>
        mkr.mk(0)
    }

  enum SUB[T, U] {
    case Refl[S]() extends SUB[S, S]
  }

  // f <: g & x <: T ==> f[x] <: g[T]
  def bar[F[_], G[_], X](fx: F[X], ksub: F KSUB G, sub: X SUB Int) =
    ksub match {
      case _: KSUB.Refl[s] =>
        sub match {
          case SUB.Refl() =>
            val gi: G[Int] = fx : s[X]
            ()
        }
    }

}
