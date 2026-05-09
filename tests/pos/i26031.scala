// https://github.com/scala/scala3/issues/26031
//
// Regression on the dotty-cps-async @ 1.3.2 community-build run: the
// `async[[A] =>> Either[E, A]] { ... }` macro call failed with [E007]
// because, when inlining a `transparent inline def` whose body returned
// `new C(using am)`, the resulting `Inlined` tree was assigned the
// avoided type `C[F, ?]` instead of `C[F, am$proxy.Context]`. The lost
// `am$proxy.Context` reference then propagated through the second
// inline layer's parameter-type prototype as a bare class projection
// `C[F, ?]#C` and made the outer context closure fail to type-check.
//
// Reproduction requires all of:
//   1. `F` is a non-trivial type lambda over a parameterized type
//      (`[A] =>> Pair[E, A]`).
//   2. The summoned `Mon[F]` is itself parametric in `E`, so its
//      static type does not refine `Context`.
//   3. Two layers of `transparent inline`: the outer constructor and an
//      `apply` that forwards to a second `transparent inline` taking
//      the path-projected `C` as its third type argument.

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
