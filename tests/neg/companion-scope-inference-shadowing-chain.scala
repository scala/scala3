// Shadowing counterpart of `tests/pos/companion-inference-chain.scala`.
// A local `Mammal` of wrong type shadows the companion-resolved
// `Animal.Mammal`, so the chained selection fails.
import scala.language.experimental.companionScopeInference

object Chain:

  sealed trait Animal
  object Animal:
    sealed trait Mammal extends Animal
    object Mammal:
      case object Dog extends Mammal

  def describe(a: Animal): String = a.toString

  // Local of unrelated type with the same name as Animal.Mammal.
  val Mammal = "warm-blooded"

  val a: String = describe(Mammal.Dog) // error
