trait Ops[F[_], A]:
  def map0[B](f0: A => B): F[B] = ???

trait Functor1[G[_]]

trait Functor2[H[_]]

trait Ref[I[_], +E]

class Test:
  given [J[_]](using J: Functor1[J]): Functor2[J] with
    extension [K](jk: J[K])
      def map2[L](f2: K => L): J[L] = ???

  def t1[
    M[_[t]],
    N[_],
  ](using N: Functor1[N]): Unit =

    val x3: Ops[N, M[[t] =>> Ref[N, t]]] = ???

    val x2: N[(M[N], M[[t] =>> Ref[N, t]])] = x3
      .map0 { refs             => (???, refs) }
      .map2 { case (not, refs) => (???, refs) }
