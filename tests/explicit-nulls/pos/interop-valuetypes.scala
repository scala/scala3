// Tests that value (non-reference) types aren't nullified by the Java transform.

class Foo {
  val x: java.lang.String = ""
  val len: Int = x.length() // type is Int and not Int|Null
}
