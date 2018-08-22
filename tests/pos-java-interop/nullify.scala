
// Tests that we add "| Null" to Java fields and methods.
class Foo {

  // So that the chained accessors compile.
  implicit def stripNull[T](x: T | Null): T = x.asInstanceOf[T]

  def foo = {
    import java.util.ArrayList
    import java.util.Iterator
    val x = new ArrayList[String]() 
    x.add(null) // | Null added to a method argument
    val y = x.get(0) 
    if (y == null) { // | Null added to return type 
    }

    val x3 = new ArrayList[ArrayList[ArrayList[String]]]() // test nested nullable containers
    if (x3.get(0) == null) {
    }
    if (x3.get(0).get(0) == null) {
    }
    if (x3.get(0).get(0).get(0) == null) {
    }

    val it = x3.iterator() // it: Iterator[Sting] | Null
    if (it == null) {
    }
  }
}
