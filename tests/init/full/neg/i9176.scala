class Foo(val opposite: Foo)
case object A extends Foo(B) // error
case object B extends Foo(A) // error
object Test {
  def main(args: Array[String]): Unit = {
    println(A.opposite)
    println(B.opposite)
  }
}