package lib

class A(val n: Int) {
  def this(c: Char) = this(c.toInt)

  val a = 30 * n

  class B(x: Int) {
    def this(c: Char) = this(c.toInt)
    val b = x * a
    def bar(i: Int) = i * x
  }

  def foo(i: Int) = i * n
}

case class Product(name: String, price: Int)