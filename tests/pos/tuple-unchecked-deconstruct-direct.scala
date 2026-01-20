class A { }
class B extends A { }
class C extends A { }

object Test {
  def foo(): (List[A], List[A]) = ???
  def bar(bs: List[B], cs: List[C]): Unit = ()

  def test =
    val (xx, yy) = foo()
    val (x: List[B @unchecked], y: List[C @unchecked]) = (xx, yy): @unchecked
    bar(x, y)
}
