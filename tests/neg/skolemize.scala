class Inv[T]

class Foo {
  val foo: Inv[this.type] = new Inv[this.type]
}
object Test {
  def test: Unit = {
    val e1 = new Foo
    val f1: Inv[Foo] = e1.foo // error
    var e2 = new Foo
    val f2: Inv[Foo] = e2.foo // error
  }
}
