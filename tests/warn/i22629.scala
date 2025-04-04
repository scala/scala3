//> using options -Wunused:all -Yno-deep-subtypes "-Wconf:msg=set repeatedly:s"

//import either.*

trait ResultMapper[A] {
  final def map[B](f: A => B): ResultMapper[B] = ???

  infix final def and[B](other: ResultMapper[B]): ResultMapper[(A, B)] = ???
}

trait BoilerplateResultMappers {

  def and[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](
    b: ResultMapper[B], c: ResultMapper[C], d: ResultMapper[D], e: ResultMapper[E], f: ResultMapper[F], g: ResultMapper[G], h: ResultMapper[H], i: ResultMapper[I], j: ResultMapper[J], k: ResultMapper[K], l: ResultMapper[L], m: ResultMapper[M], n: ResultMapper[N], o: ResultMapper[O], p: ResultMapper[P], q: ResultMapper[Q], r: ResultMapper[R], s: ResultMapper[S], t: ResultMapper[T], u: ResultMapper[U], v: ResultMapper[V], w: ResultMapper[W]
  ): ResultMapper[(B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)] =
    (b and c and d and e and f and g and h and i and j and k and l and m and n and o and p and q and r and s and t and u and v and w).map {
      case ((((((((((((((((((((((b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v), w) =>
      (b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)
    }
}

/*
object either {
  type ResultMapperException = RuntimeException
  implicit class EitherOps[A](private val ea: Either[ResultMapperException, A]) extends AnyVal {
    def and[B](eb: Either[ResultMapperException, B]): Either[ResultMapperException, (A, B)] =
      (ea, eb) match {
        case (Right(a), Right(b)) =>
          Right((a, b))

        case (Right(_), Left(ex)) =>
          Left(ex)

        case (Left(ex), Right(_)) =>
          Left(ex)

        case (Left(_), Left(_)) =>
          Left(RuntimeException())
      }
  }
}
*/
