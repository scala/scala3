// Test what can be compared for equality against null.
class Foo {
  // Null itself
  val x0: Null = null
  x0 != x0
  x0 == null
  x0 != null
  null == x0
  null == null
  null != null

  // Non-nullable types: error
  val x1: String = "hello"
  x1 != null  // error
  x1 == null  // error
  null == x1  // error
  null != x1  // error
  x1 == x0    // error
  x0 != x1    // error
  x1.asInstanceOf[String|Null] == null
  x1.asInstanceOf[String|Null] == x0
  x1.asInstanceOf[Any] == null
  x1.asInstanceOf[Any] == x0

  // Nullable types: OK
  val x2: String|Null = null
  x2 == null
  null == x2
  x2 == x0
  x2 != x0
  x0 == x2
  x2 == x1
  x2 != x1
  x1 == x2

  // Value types: not allowed.
  1 == null     // error
  null != 0     // error
  null == 0     // error
  true == null  // error
  null == false // error
  'a' == null   // error
  null == 'b'   // error
}
