class Foo

object Test {
  implicit def two(x: Foo): Two = new Two(x)

  class Two(x: Foo) {
    private def meth: Unit = {}

    def test2(foo: Foo): Unit = {
      foo.meth // error
    }
  }
}


object Test2 {

  class Two(x: Foo) {
    implicit def two(x: Foo): Two = new Two(x)

    private def meth: Unit = {}

    def test2(foo: Foo): Unit = {
      foo.meth // error
    }
  }
}
