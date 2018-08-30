
// Test that we mark fields and methods in Java classfiles as nullable.
class Foo {

  def foo = {
    import java.util.ArrayList
    val x = new ArrayList[String]()
    x.add("Hello") // Allowed since `add` takes String|JavaNull as argument
    x.add(null)
    val r: String = x.get(0) // error: got String|JavaNull instead of String

    val x2 = new ArrayList[Int]()
    val r2: Int = x2.get(0) // error: even though Int is non-nullable in Scala, its counterpart
    // (for purposes of generics) in Java (Integer) is. So we're missing |JavaNull

    // Test that as we extract return values, we're missing the |JavaNull in the return type.
    // i.e. test that the nullability is propagated to nested containers.
    val ll = new ArrayList[ArrayList[ArrayList[String]]]
    val level1: ArrayList[ArrayList[String]] = ll.get(0) // error
    val level2: ArrayList[String] = ll.get(0).get(0) // error
    val level3: String = ll.get(0).get(0).get(0) // error
    val ok: String = ll.get(0).get(0).get(0) // error

    // Test that return values in PolyTypes are marked as nullable.
    val lstring = new ArrayList[String]()
    val res: String = java.util.Collections.max(lstring) // error: missing |Null
    val res2: String|Null = java.util.Collections.max(lstring) // ok
  }
}
