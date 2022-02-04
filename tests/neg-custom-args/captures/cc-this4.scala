open class C: // error
  val x: C = this

open class D:
  this: D =>
  val x: D = this

