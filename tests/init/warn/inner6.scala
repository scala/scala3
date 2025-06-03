class Parent {
  class Inner1 {
    val len = list.size
  }

  class Inner2 {
    val len = foo
  }

  private val list = List(3, 5, 6)
  def foo: Int = 5
}

class Child extends Parent {
  class InnerA extends Inner1
  class InnerB extends Inner2

  new InnerA
  new InnerB

  val x = 10       // warn
  override def foo: Int = x * x
}
