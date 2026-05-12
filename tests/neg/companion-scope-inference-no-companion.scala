import scala.language.experimental.companionScopeInference

// A type without a companion module cannot rescue a missing identifier.
object NoCompanion:

  trait Bare // no companion module

  val x: Bare = Foo // error

  // Function types fall through `Function1`'s companion which has no
  // useful members.
  val f: Int => String = NotThere // error
