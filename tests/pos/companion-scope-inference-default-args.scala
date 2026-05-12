// SIP-80 §Overload resolution: overloads that differ only by an extra
// default-valued parameter resolve a bare-ident argument against the parameter
// type they share.
import scala.language.experimental.companionScopeInference

sealed trait Animal
object Animal:
  case object Cat extends Animal
  case object Dog extends Animal

object SameFirstFormal:
  def bar(a: Animal): Int                 = 1
  def bar(a: Animal, b: Animal = Dog): Int = 2

  // Both overloads applicable by arity. Shared formal at position 0 is
  // `Animal`, so `Cat` (not in scope) resolves via the companion.
  val r: Int = bar(Cat)

  // Common formal at position 0 with a shorthand followed by an explicit
  // second arg should also work — the 2-arg overload is selected.
  val r2: Int = bar(Cat, Dog)
