trait Ctx[F[_]]

trait Mon[F[_]]:
  type Context <: Ctx[F]

object Mon:
  type Aux[F[_], C <: Ctx[F]] = Mon[F] { type Context = C }

class Wrap[F[_], C <: Ctx[F]](using val am: Mon.Aux[F, C]):
  transparent inline def apply[T](inline expr: C ?=> T): F[T] =
    inner[F, T, C](am, expr)

transparent inline def inner[F[_], T, C <: Ctx[F]](
    inline am: Mon.Aux[F, C],
    inline expr: C ?=> T
): F[T] = ???

transparent inline def doIt[F[_]](using am: Mon[F]) =
  new Wrap(using am)

class Pair[A, B]
class PairCtx[E] extends Ctx[[A] =>> Pair[E, A]]
class PairMon[E] extends Mon[[A] =>> Pair[E, A]]:
  type Context = PairCtx[E]

given pm[E]: Mon[[A] =>> Pair[E, A]] = new PairMon[E]

object Test:
  val r = doIt[[A] =>> Pair[String, A]] { 42 }
