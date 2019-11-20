// Test what can be compared for equality against null.
class Foo {
  // Null itself
  val x0: Null = null
  x0 == null
  x0 != null
  null == x0
  null == null

  // Nullable types: OK
  val x1: String|Null = null
  x1 == null
  null == x1
  x1 == x0
  x1 != x0
  x0 == x1

  // Reference types, even non-nullable ones: OK.
  // Allowed as an escape hatch.
  val x2: String = "hello"
  x2 != null
  x2 == null
  null == x2

  // Value types: not allowed.
  1 == null     // error
  null != 0     // error
  null == 0     // error
  true == null  // error
  null == false // error
  'a' == null   // error
  null == 'b'   // error
}
