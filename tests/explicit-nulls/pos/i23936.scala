//> using options -Yexplicit-nulls

sealed abstract class IsSubtypeOfOutput[-A, +B] extends (A => B)
object IsSubtypeOfOutput:
  private val instance: IsSubtypeOfOutput[Any, Any] = new IsSubtypeOfOutput[Any, Any] { def apply(a: Any): Any = a }

sealed trait DerivationAnnotation
class Rename(name: String) extends DerivationAnnotation
