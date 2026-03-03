class A { }
class B extends A { }
class C extends A { }

object Test {
  def foo(): (List[A], List[A]) = ???
  def bar(bs: List[B], cs: List[C]): Unit = ()

  def test =
    val (x: List[B @unchecked], y: List[C @unchecked]) = foo(): @unchecked
    bar(x, y)
}
