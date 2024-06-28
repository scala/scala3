

class Foo {

  def foo1: Unit = 2 // warn: A pure expression does nothing in statement position

  def foo2: Unit = {
    3 // warn: A pure expression does nothing in statement position
    4 // warn: A pure expression does nothing in statement position
  }
}