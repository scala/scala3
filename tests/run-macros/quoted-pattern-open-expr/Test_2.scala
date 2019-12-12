
object Test {
  def main(args: Array[String]): Unit = {
    println(test { val x: Int = 4; 6: Int })
    println(test { val x: Int = 4; x * x })
    println(test { val f: Int => Int = x => x + 1; f(3) })
    println(test { def f(x: Int): Int = 5; f(5) })
    println(test { def f(x: Int): Int = x; f(5) })
  }
}
