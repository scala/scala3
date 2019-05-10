class A extends (Int => i1) // error
class B extends (Int => this) // error
trait C {
  val bar: Int => this // error
}

// Test that function types ending in SIP-23 singleton types are understood correctly.

class D extends (Int => 1) {
  def apply(x: Int) = 2 // error
}

class Wrap(x: Int)
class E extends (Wrap)( // error // error
// error