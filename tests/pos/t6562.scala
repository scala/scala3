class Test {

  rewrite def foo: Unit = {
    def it = new {}
    (_: Any) => it
  }

  rewrite private def bar: Unit = {
    def it = new {}
    (_: Any) => it
  }
}
