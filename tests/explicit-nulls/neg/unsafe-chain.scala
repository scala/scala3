// Test that we can select through "| Null" is unsafeNulls is enabled (unsoundly).

class Foo {
  import java.util.ArrayList
  import java.util.Iterator

  val x3 = new ArrayList[ArrayList[ArrayList[String]]]()
  val x4: Int = x3.get(0).get(0).get(0).length() // error
}
