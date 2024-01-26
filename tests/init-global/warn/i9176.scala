class Foo(val opposite: Foo)
case object A extends Foo(B) // warn
case object B extends Foo(A)
object Test {
  def main(args: Array[String]): Unit = {
    println(A.opposite)
    println(B.opposite)
  }
}
