class Parent {
  @scala.annotation.filled
  class Inner1 {
    val len = foo   // error
  }

  val list = List(3, 5, 6)
  def foo: Int = 5
}

class Child extends Parent {
  class InnerA extends Inner1

  new InnerA
}
