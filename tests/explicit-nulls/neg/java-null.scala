// Test that `UncheckedNull` is see-through, but `Null` isn't.

class Test {
  val s: String|Null = "hello"
  val l = s.length // error: `Null` isn't "see-through"

  val s2: String|UncheckedNull = "world"
  val l2 = s2.length // ok
}

