package i26031multi

trait Ctx[F[_]]
trait Key[F[_]]

trait Mon[F[_]]:
  type Context <: Ctx[F]
  type Token <: Key[F]

object Mon:
  type Aux[F[_], C <: Ctx[F], K <: Key[F]] = Mon[F] { type Context = C; type Token = K }

class Wrap[F[_], C <: Ctx[F], K <: Key[F]](using val am: Mon.Aux[F, C, K]):
  transparent inline def apply[T](inline expr: (C, K) ?=> T): F[T] =
    inner[F, T, C, K](am, expr)

transparent inline def inner[F[_], T, C <: Ctx[F], K <: Key[F]](
    inline am: Mon.Aux[F, C, K],
    inline expr: (C, K) ?=> T
): F[T] = ???

transparent inline def doIt[F[_]](using am: Mon[F]) =
  new Wrap(using am)

class Pair[A, B]
class PairCtx[E] extends Ctx[[A] =>> Pair[E, A]]
class PairKey[E] extends Key[[A] =>> Pair[E, A]]
class PairMon[E] extends Mon[[A] =>> Pair[E, A]]:
  type Context = PairCtx[E]
  type Token = PairKey[E]

given pm[E]: Mon[[A] =>> Pair[E, A]] = new PairMon[E]

object Test:
  val r = doIt[[A] =>> Pair[String, A]] { 42 }
