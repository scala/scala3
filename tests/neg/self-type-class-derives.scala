trait TC:
  type Self
  def apply(): Self

object TC:
  def derived[X]: TC { type Self = X } = ???

// Without the `modularity` feature enabled, a self-based type class in a
// `derives` clause is rejected exactly as before this proposal.
case class NoModularity() derives TC // error

object WithModularity:
  import scala.language.experimental.modularity

  // Derivation succeeds here; both spellings of the resulting type refer
  // to the same shape, `TC { type Self = Mono }`.
  case class Mono() derives TC
  val ok1: Mono is TC = summon[Mono is TC]
  val ok2: TC { type Self = Mono } = summon[TC { type Self = Mono }]

  // Higher-kinded ADT type parameters have no counterpart to unify against
  // for a self-based type class (there is no case (a) here), so this fails
  // exactly like an ordinary kind-* type class would.
  case class HK[F[_]](fa: F[Int]) derives TC // error
