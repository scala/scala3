// SIP-80 chain semantics: only the leftmost identifier uses companion
// inference. `Dog` is a member of `Animal.Mammal`'s companion, NOT of
// `Animal`'s companion, so the bare `Dog` at an `Animal`-typed position
// cannot be inferred. The user must chain through `Mammal.Dog` or qualify.
import scala.language.experimental.companionScopeInference

object Chain:

  sealed trait Animal
  object Animal:
    sealed trait Mammal extends Animal
    object Mammal:
      case object Dog extends Mammal

  def describe(a: Animal): String = a.toString

  // `Dog` is not in `Animal`'s companion. Supertype-companion search is
  // intentionally not performed. Diagnostic: "Not found: Dog".
  val a: String = describe(Dog) // error
