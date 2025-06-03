open class C:
  val x: C = this  // error

open class D:
  this: D =>
  val x: D = this

