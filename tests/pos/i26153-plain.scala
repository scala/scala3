// Plain-inline variant of #26153.
trait Ctx[F[_]]

trait Mon[F[_]]:
  type Context <: Ctx[F]

class Wrap[F[_], C <: Ctx[F]]:
  inline def apply[T](inline expr: C ?=> T): F[T] =
    stage2[F, T, C](expr)

inline def stage2[F[_], T, C <: Ctx[F]](inline expr: C ?=> T): F[T] = ???

inline def async[F[_]](using am: Mon[F]) =
  new Wrap[F, am.Context]

class TupleMon[E] extends Mon[[A] =>> (E, A)]:
  type Context = Ctx[[A] =>> (E, A)]

given pm[E]: Mon[[A] =>> (E, A)] = new TupleMon[E]

object Test:
  val r = async[[A] =>> (String, A)] { 42 }
