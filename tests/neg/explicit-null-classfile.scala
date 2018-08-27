
// Test that we mark fields and methods in Java classfiles as nullable.
class Foo {

  def s[T](x: T | Null): T = x.asInstanceOf[T]

  def foo = {
    import java.util.ArrayList
    val x = new ArrayList[String]()
    x.add("Hello") // Allowed since `add` takes String | Null as argument
    x.add(null)
    val r: String = x.get(0) // error: got String | Null instead of String
    val ll = new ArrayList[ArrayList[ArrayList[String]]]
    val level1: ArrayList[ArrayList[String]] = ll.get(0) // error
    val level2: ArrayList[String] = s(ll.get(0)).get(0) // error
    val level3: String = s(s(ll.get(0)).get(0)).get(0) // error
    val ok: String = s(s(s(ll.get(0)).get(0)).get(0))
  }

}
