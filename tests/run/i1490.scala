class Base {
  type T = Int | Boolean
  def test(x: Object) = x.isInstanceOf[T]
}

object Test {
  def main(args: Array[String]) = {
    val b = new Base
    println(b.test(Int.box(3)))
    println(b.test(Boolean.box(false)))
    println(b.test(Double.box(3.4)))
  }
}