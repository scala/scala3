// Test that reference types are no longer nullable.

class Foo {
  val s: String = null        // error
  val s1: String|Null = null  // ok
  val b: Boolean = null       // error
  val ar: AnyRef = null       // error
  val a: Any  = null          // ok
  val n: Null = null          // ok
}

