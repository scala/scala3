
// Test that JavaNull can be assigned to Null.
class Foo {
  import java.util.ArrayList
  val l = new ArrayList[String]()
  val s: String = l.get(0) // error: return type is nullable
  val s2: String|Null = l.get(0) // ok
}
