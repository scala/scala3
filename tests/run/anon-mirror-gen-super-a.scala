import scala.deriving.Mirror

@main def Test = {

  class Foo {
    final val foo: Qux.type = Qux
    case object Qux
  }

  class Bar extends Foo {
    val mQux = summon[Mirror.Of[Bar.super.foo.type]]
    assert(mQux.fromProduct(EmptyTuple) == Qux)
  }

}
