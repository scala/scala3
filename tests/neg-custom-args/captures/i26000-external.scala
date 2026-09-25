package i26000external
// Symbols in the empty package are exempt from explicit-type checks,
// so this test needs a package declaration.

import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

object Lib:
  // An externally visible definition whose inferred type embeds a
  // capture-polymorphic lambda needs an explicit type, like for a lambda
  // that appears directly on the right-hand side of the definition.
  // Before the fix for issue #26000 this compiled, but with all capture
  // sets referring to `C` erased to `{}` in the externally visible type.
  val f = Box([C^] => (x: File^{C}) => x) // error

  def g = Box([C^] => (xs: List[File^{C}]) => xs) // error

  val ok: Box[[C^] => (x: File^{C}) -> File^{C}] =
    Box([C^] => (x: File^{C}) => x)
