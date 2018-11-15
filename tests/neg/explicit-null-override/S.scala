class S extends J {
  override def foo(x: String): Unit = {}
  override def bar(x: C[C[C[String]]]): Unit = {}
}

class S2 extends J {
  override def foo(x: String|Null): Unit = {}
  override def bar(x: C[C[C[String|Null]|Null]|Null]|Null): Unit = {}
}

class Base {
  def foo(x: String): Unit = {}
  def bar(x: String|Null): Unit = {}
}

class Derived extends Base {
  override def foo(x: String|Null): Unit = {} // error: can't ignore null when extending from Scala
  override def bar(x: String): Unit = {}      // error: can't ignore null when extending from Scala
}
