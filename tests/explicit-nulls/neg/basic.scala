// Test that reference types are no longer nullable.

class Foo {
  val s: String = null // error
  val s1: String | Null = null
  val s2: String | Null = ""

  val b: Boolean = null // error
  val c: Int | Null = null

  val ar1: AnyRef = null // error
  val ar2: AnyRef | Null = null
  val ob: Object = null // error

  val av: AnyVal = null // error
  val a: Any  = null
  val n: Null = null
}
