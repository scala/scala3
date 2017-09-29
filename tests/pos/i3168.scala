object Test {
  class C {
    def foo(x: Int) = 1
    def foo(x: Double) = 2
  }

  implicit class COps(val x: C) {
    def foo(x: String) = 3
  }

  def test: Unit = {
    (new C).foo("Hello")
  }
}
