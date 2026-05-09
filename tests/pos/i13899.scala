// https://github.com/scala/scala3/issues/13899
//
// Failed to bind context-depended type into Aux when matcher is inline.
// The body `InferAsyncArg(am)` requires `am: ContextMonad.Aux[F, C]` for
// some C; with `using inline am: ContextMonad[F]`, C should be inferred
// to `am.Context` (a path-dependent type member of the using-inline
// parameter), but historically `am` was flagged unstable for path-dependent
// typing because `isStableMember` excluded `InlineParam`. Once that
// stability is restored, the InlinerMap also needs to substitute the
// `am.type` singleton with the inline-bound proxy's TermRef instead of
// widening it (the previous unconditional widening was correct *only*
// while inline params were considered unstable).

package x

trait ContextMonad[F[_]]:
  type Context

object ContextMonad:
  type Aux[F[_], C] = ContextMonad[F] { type Context = C }

trait CBContext:
  def foo() = ???

trait CB[T]

given ContextMonad[CB] with
  override type Context = CBContext

inline def runContext[F[_]](using inline am: ContextMonad[F]) = InferAsyncArg(am)

class InferAsyncArg[F[_], C](val am: ContextMonad.Aux[F, C]):
  def apply[T](f: C ?=> T): F[T] = ???

object X:
  def bar = runContext[CB] {
    summon[CBContext].foo()
    1
  }
