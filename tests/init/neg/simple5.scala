class Foo {
  def b = {
    def bar = name.size
    bar
  }

  b

  val name = "Jack"   // error
}

class Bar {
  def b = {
    lazy val m = name.size
    m
  }

  b

  val name = "Jack"   // error
}