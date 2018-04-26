object Test {

  def takesPartialFunction(a: PartialFunction[Int, Int]) = a(1)
  class Foo(val field: Option[Int])

  def main(args: Array[String]): Unit = {
    val p1: PartialFunction[Int, Int] = { case a: Int => a }
    assert(takesPartialFunction(p1) == 1)

    val p2: PartialFunction[Foo, Int] =
      foo => foo.field match { case Some(x) => x }
    assert(p2.isDefinedAt(new Foo(Some(1))))
    assert(!p2.isDefinedAt(new Foo(None)))
  }
}
