// Test for issue #24917: Named tuple type in pattern type ascription
// The pattern `_: (a: Int, b: String)` should be valid - it's a type annotation
// with a named tuple type, not a named pattern.

object Test {
  val ab: (a: Int, b: String) = (42, "hello, world")

  // Basic case from the issue
  ??? match {
    case _: (a: Int, b: String) => ???
  }

  def `named tuple type in pattern with binding` =
    ab match
    case x: (a: Int, b: String) => x.b * x.a

  // Named tuple type in pattern with binding
  ??? match {
    case x: (a: Int, b: String) => ???
  }

  // Single element named tuple type
  ??? match {
    case _: (name: String) => ???
  }

  // Three element named tuple type
  ??? match {
    case _: (x: Int, y: Int, z: Int) => ???
  }

  // Named tuple type with different types
  ??? match {
    case _: (flag: Boolean, count: Long, data: List[Int]) => ???
  }

  // Named tuple type in nested match
  ??? match {
    case outer @ (_: (inner: (a: Int, b: String))) => ???
  }

  // Named tuple type with type parameters
  def foo[T]: Unit = {
    ??? match {
      case _: (elem: T) => ???
    }
  }

  // Multiple patterns with named tuple types
  ??? match {
    case _: (a: Int) => ???
    case _: (a: Int, b: Int) => ???
    case _: (a: Int, b: Int, c: Int) => ???
    case _ => ???
  }

  // Named tuple type in val pattern
  val _: (x: Int, y: Int) = ???

  // Named tuple type as function parameter pattern
  def bar(t: (a: Int, b: String)): Int = t match {
    case _: (a: Int, b: String) => 1
  }
}

// Ensure existing named pattern functionality still works
object NamedPatternTest {
  type Person = (name: String, age: Int)
  val person: Person = (name = "Alice", age = 30)

  // Named patterns in tuple destructuring (existing functionality)
  val (name = n, age = a) = person

  // Named patterns in match (existing functionality)
  person match {
    case (name = x, age = y) => println(s"$x is $y years old")
  }
}
