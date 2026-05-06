import scala.language.experimental.targetTypedCompanionShorthand

// A type without a companion module (here: a refinement of a trait) cannot
// resolve `.X`.
object NoCompanion:

  trait Bare // no companion module

  val x: Bare = .Foo // error

  // Function types fall through `Function1`'s companion which has no
  // user-declared cases — useful members (`apply`) won't match a value-type
  // expectation.
  val f: Int => String = .NotThere // error
