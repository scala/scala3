class Test {

  transparent def foo: Unit = {
    def it = new {}
    (_: Any) => it
  }

  transparent private def bar: Unit = {
    def it = new {}
    (_: Any) => it
  }
}
