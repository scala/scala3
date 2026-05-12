// Shadowing counterpart of `tests/pos/companion-inference-default-args.scala`.
// A local `Cat` of the wrong type wins normal resolution, so the
// pre-typing pass in overload resolution feeds the local value at the
// shared formal position — the result is a type mismatch.
import scala.language.experimental.companionScopeInference

sealed trait Animal
object Animal:
  case object Cat extends Animal
  case object Dog extends Animal

object SameFirstFormal:
  def bar(a: Animal): Int                         = 1
  def bar(a: Animal, b: Animal = Animal.Dog): Int = 2

  // Local of unrelated type with the same name as Animal.Cat.
  val Cat = "feline"

  val r: Int = bar(Cat) // error
