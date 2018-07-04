object Test {
  trait Bar
  trait Foo extends Bar

  transparent def test(t: Any) =
    t match {
      case x: Bar => "a"
      case _ => 1
    }

  // def f1[T <: Foo](t: T): "a" = test(t) // Do we want this?
  //def f1[T <: Foo](t: T): Any = test(t)
  def f2(x: Foo): "a"  = test(x)
  // def f3(x: String): 1 = test(x)
}
