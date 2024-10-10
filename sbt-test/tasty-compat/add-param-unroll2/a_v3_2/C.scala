package a
import b.*

// C is a separate compilation unit to A, and inlines B.caller, so its TASTy will try to resolve
// the generated A.foo forwarder, which will not exist yet in typer phase.
// The unit will suspend, and in the second run, A.foo will be generated, so resolution will succeed.
object C {
  val res: String = B.caller
}
