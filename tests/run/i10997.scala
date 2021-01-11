class ClassWrapper {

  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent
  case object Qux extends Parent

  def testcase(): Unit =

    val mirrorParent = summon[deriving.Mirror.Of[Parent]]
    val mirrorFoo    = summon[deriving.Mirror.Of[Foo]]
    val mirrorBar    = summon[deriving.Mirror.Of[Bar]]
    val mirrorQux    = summon[deriving.Mirror.Of[Qux.type]]

    val fooShapedTuple: (Int, Int, String) = (23, 47, "ok")
    val barShapedTuple: (Int, Int)         = (57, 71)
    val quxShapedTuple: EmptyTuple         = EmptyTuple

    val foo: Foo      = mirrorFoo.fromProduct(fooShapedTuple)
    val bar: Bar      = mirrorBar.fromProduct(barShapedTuple)
    val qux: Qux.type = mirrorQux.fromProduct(quxShapedTuple)

    assert(foo == Foo(23, 47, "ok"))
    assert(bar == Bar(57, 71))
    assert(qux == Qux)

    val fooOrd = mirrorParent.ordinal(foo)
    val barOrd = mirrorParent.ordinal(bar)
    val quxOrd = mirrorParent.ordinal(qux)

    assert(fooOrd == 0)
    assert(barOrd == 1)
    assert(quxOrd == 2)
}

@main def Test =
  ClassWrapper().testcase()
