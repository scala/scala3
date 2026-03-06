import scala.compiletime.summonInline

trait MyEvidence[T]

object Scope:
  opaque type T = String

  inline given MyEvidence[T] with {}

inline def summonInlineNoOpProxy: MyEvidence[Scope.T] = summonInline[MyEvidence[Scope.T]]

val smoke = (
  summonInline[MyEvidence[Scope.T]],
  summonInlineNoOpProxy,
)
