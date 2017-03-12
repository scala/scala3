class Foo

object Test {
  implicit class One(x: Foo) {
    def meth: Unit = {}
  }

  implicit class Two(x: Foo) {
    private def meth: Unit = {}
  }

  def test(foo: Foo): Unit = {
    foo.meth
  }
}
