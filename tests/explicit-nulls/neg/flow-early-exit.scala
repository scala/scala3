// Test that flow-sensitive type inference handles early exits from blocks.

class Foo(x: String|Null) {

  // Test within constructor
  if (x == null) throw new NullPointerException()
  val x2: String = x // error: flow inference for blocks doesn't work inside constructors

  def foo(): Unit = {
    val y: String|Null = ???
    if (y == null) return ()
    val y2: String = y // ok
  }

  def bar(): Unit = {
    val y: String|Null = ???
    if (y != null) {
    } else {
      return ()
    }
    val y2: String = y // ok
  }

  def fooInExprPos(): String = {
    val y: String|Null = ???
    if (y == null) return "foo"
    y // ok
  }

  def nonLocalInBlock(): String = {
    val y: String|Null = ???
    if (y == null) { println("foo"); return "foo" }
    y
  }

  def barWrong(): Unit = {
    val y: String|Null = ???
    if (y != null) {
      return ()
    } else {
    }
    val y2: String = y // error: can't infer that y is non-null (actually, it's the opposite)
  }

  def err(msg: String): Nothing = {
    throw new RuntimeException(msg)
  }

  def retTypeNothing(): String = {
    val y: String|Null = ???
    if (y == null) err("y is null!")
    y
  }

  def errRetUnit(msg: String): Unit = {
    throw new RuntimeException(msg)
    ()
  }

  def retTypeUnit(): String = {
    val y: String|Null = ???
    if (y == null) errRetUnit("y is null!")
    y // error: previous statement returned unit so can't infer non-nullability
  }
}
