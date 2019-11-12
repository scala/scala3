// Test reference equality.

class Test {
  val x1: String = "hello"
  val x2: String|Null = null

  // Methods in AnyRef
  x1.eq(x1)
  x1.ne(1)
  x1.eq(null)
  x1.ne(null)
  x1.eq(1) // ok: implicit conversion from int to Integer
  x1.ne(1) // ok: ditto
  x1.eq(x2)
  x1.ne(x2)

  // Extension methods
  null.eq("hello")
  null.ne("hello")
  null.eq(null)
  null.ne(null)
  null.eq(x2)
  null.ne(x2)

  x2.eq(null)
  x2.ne(null)
  x2.eq(x1)
  x2.ne(x1)
  x2.eq(x2)
  x2.ne(x2)
}
