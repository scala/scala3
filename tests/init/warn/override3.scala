trait Foo {
  println("init x")
  val x = "world"
  val y = foo(5)

  def foo(n: Int): String
}

class Bar1 extends Foo {
  val m = "hello"

  def foo(n: Int) = "world"
}

class Qux extends Bar1 {
  val u = "hello"            // warn

  override def foo(n: Int) = u + "world"
}

class Bar2 extends Foo {
  val m = "hello"

  final def foo(n: Int) = "world"
}

class Bar3(m: String) extends Foo {
  final def foo(n: Int) = m + "world"
}
