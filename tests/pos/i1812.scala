class FF[R] {
  def compose(): R = ???
}

class Test(x: Int) extends AnyVal {
  def method: Unit = {
    class Bla
    class Foo extends FF[Bla] {
      override def compose() = super[FF].compose()
    }
  }
}
