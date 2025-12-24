trait Inliner[A]:
    inline def apply[T]: A

class SummonInliner[F[_]] extends Inliner[ForSome[F]]:
    inline def apply[T]: ForSome[F] = ForSome(compiletime.summonInline[F[T]])

type ForSome[F[_]] = ForSome.Type[F]
object ForSome:
    type Type[F[_]] = Unwrap[F, ?]
    class Unwrap[F[_], A](val unwrap: F[A]) extends AnyVal

    inline def apply[F[_], A](v: F[A]): Type[F] = Unwrap(v)
