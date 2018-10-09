object Test {
  trait Bar
  trait Foo extends Bar

  trait Baz

//  object Unapp {
//    dependent def unapply(foo: Foo): Boolean = true
//  }
  dependent case class Tup(m: Int, n: Int)

  dependent def test(t: Any, i: Int) =
    t match {
      case x: Baz => "z"
      case x: Bar if i > 0 => "a"
//      case Unapp() => "a"
//      case Tup(_, _) => "a"
      case _ => 1
    }

  dependent def test2(n: Int) =
    n match {
      case 1 => "a"
      case 2 => "b"
    }

  // def f1[T <: Foo](t: T): "a" = test(t) // Do we want this?
  // def f1[T <: Foo](t: T): Any = test(t)
  def f2(x: Foo): "a" = test(x, 123)
  // def f3(x: String): 1 = test(x)
  test2(1): "a"
  test2(2): "b"
}
