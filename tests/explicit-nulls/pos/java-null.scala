// Test that `UncheckedNull`able unions are transparent
// w.r.t member selections.

class Test {
  val s: String|UncheckedNull = "hello"
  val l: Int = s.length // ok: `UncheckedNull` allows (unsound) member selections.

  val s2: UncheckedNull|String = "world"
  val l2: Int = s2.length

  val s3: UncheckedNull|String|UncheckedNull = "hello"
  val l3: Int = s3.length

  val s4: (String|UncheckedNull)&(UncheckedNull|String) = "hello"
  val l4 = s4.length
}
