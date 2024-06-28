

class Foo {

  val a: Int = 3

  def b: 1 = {
    println("1")
    1
  }

  val c: Unit = ()

  def foo1: Unit = a // warn: A pure expression does nothing in statement position
  def foo2: Unit = b
  def foo3: Unit = c // Not addapted to { c; () } and hence c is not a statement

}

