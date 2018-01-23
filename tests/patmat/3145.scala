object Test {
  sealed trait Foo
  class Bar(val s: String) extends Foo
  sealed abstract class Baz(val s: String) extends Foo

  val f: Foo => String = {
    case bar: Bar => bar.s
    case baz: Baz => baz.s
  }
}

object Test2 {
  sealed trait Foo
  class Bar extends Foo
  sealed trait Baz extends Foo

  def f(x: Foo) = x match {
    case bar: Bar => 1
    case baz: Baz => 2
  }
}

object Test3 {
  sealed trait Foo
  class Bar extends Foo
  sealed trait Baz extends Foo

  def foo = {
    val x: Foo = new Baz {}
    x match {
      case bar: Bar => 1
      case baz: Baz => 2
    }
  }

  def bar = {
    val x: Baz  = new Baz {}
    x match {
      case bar: Bar => 1
      case baz: Baz => 2
    }
  }
}