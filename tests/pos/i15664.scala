trait CpsMonad[F[_]]:
  type Context <: CpsMonadContext[F]
type Aux[F[_], C <: CpsMonadContext[F]] = CpsMonad[F] { type Context = C }
trait CpsMonadContext[F[_]]
trait CpsMonadInstanceContext[F[_]] extends CpsMonad[F]:
  type Context = CpsMonadInstanceContextBody[F]
class CpsMonadInstanceContextBody[F[_]](m: CpsMonadInstanceContext[F]) extends CpsMonadContext[F]
class InferAsyncArg[F[_], C <: CpsMonadContext[F]](using val am: Aux[F, C])

sealed abstract class ZManaged[-R, +E, +A]
type RManaged[-R, +A] = ZManaged[R, Throwable, A]

type ForZManaged[R, E] = [X] =>> ZManaged[R, E, X]
given zManagedCpsMonad[R, E]: CpsMonadInstanceContext[ForZManaged[R, E]] = ???

// Usage
def failing[R, E](using
    CpsMonad[ForZManaged[R, E]]
): InferAsyncArg[ForZManaged[R, E], CpsMonadInstanceContextBody[ForZManaged[R, E]]] =
  new InferAsyncArg()

def compiling[R, E](using
    CpsMonad[ForZManaged[R, E]]
) =
  new InferAsyncArg[ForZManaged[R, E], CpsMonadInstanceContextBody[ForZManaged[R, E]]]
