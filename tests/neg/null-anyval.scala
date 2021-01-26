object Test:
  val x: Int = 0
  val y: Int | Null = x // during erasure, x is boxed here, and Int | Null becomes Object
  val z0: Int = identity(y) // error
  val z1: Int = identity[Int |  Null](y) // error
  val z2: Int = y // error

  class StrWrapper(x: String) extends AnyVal
  val z3: StrWrapper = null  // error
  val z4: O.T = null // error
object O:
  opaque type T = String
