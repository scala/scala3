class A { }
class B extends A { }

object Test {
  def foo(): (List[A], List[A]) = ???
  def bar(bs: List[B]): Unit = ()

  def test =
    val (x: List[B @unchecked], _) = foo(): @unchecked
    bar(x)
}
