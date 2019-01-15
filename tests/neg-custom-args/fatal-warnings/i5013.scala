class Foo {

  def foo1: Unit = 2 // error: A pure expression does nothing in statement position

  def foo2: Unit = {
    3 // error: A pure expression does nothing in statement position
    4 // error: A pure expression does nothing in statement position
  }
}
