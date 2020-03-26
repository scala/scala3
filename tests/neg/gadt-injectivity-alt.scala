object test {

  enum SUB[-F, +G] {
    case Refl[S]() extends SUB[S, S]
  }

  enum KSUB[-F[_], +G[_]] {
    case Refl[S[_]]() extends KSUB[S, S]
  }

  def foo[F[_], G[_], A](
    keq: (F KSUB Option, Option KSUB F),
    ksub: Option KSUB G,
    sub: F[A] SUB G[Int],
    a: A
  ) =
    keq._1 match { case KSUB.Refl() =>
      keq._2 match { case KSUB.Refl() =>
        ksub match { case KSUB.Refl() =>
          sub match { case SUB.Refl() =>
            //   f    =  Option
            // & g    >: Option
            // & f[a] <: g[I]
            // =X=>
            // a <: I
            // counterexample: g = [t] => Any
            val i: Int = a // error
            ()
          }
        }
      }
    }
}
