//> using options -Wunused:all -Werror

trait MonadError[F[_], E]
type MonadThrow[F[_]] = MonadError[F, Throwable]

trait MetaStreams[F[_]]:
  def use[A]: F[A] = ???
trait WriteResult

trait MetaStreamsSyntax:
  extension [F[_]](ms: MetaStreams[F])(using MonadThrow[F])
    def setMaxAge(): F[WriteResult] =
      summon[MonadThrow[F]]
      ms.use[WriteResult]

    def setTruncateBefore(): F[WriteResult] =
      ms.use[WriteResult]
