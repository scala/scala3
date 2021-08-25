/** A modified version of gadt-injectivity-alt.scala. */
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
    sub: F[Option[A]] SUB G[Option[Int]],
    a: A
  ) =
    keq._1 match { case KSUB.Refl() =>
      keq._2 match { case KSUB.Refl() =>
        ksub match { case KSUB.Refl() =>
          sub match { case SUB.Refl() =>
            //        F        =  Option
            // &      G       >: Option
            // & F[Option[A]] <: G[Option[Int]]
            // =X=>
            // A <: Int
            //
            // counterexample: G = [T] => Any
            val i: Int = a // error
            ()
          }
        }
      }
    }
}
