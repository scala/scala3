// Check that the `.nn` extension method strips away nullability.

class Test {
  val s1: String|Null = ???
  val s2: String = s1.nn

  type NString = String|Null
  val s3: NString = ???
  val s4: String = s3.nn

  // `.nn` is a no-op when called on value types
  val b1: Boolean = true
  val b2: Boolean = b1.nn

  // Check that `.nn` interacts well with type inference.
  def foo(s: String): String = s
  val s5: String|Null = "hello"
  val s6 = s5.nn
  foo(s6)
}
