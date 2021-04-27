import api.*

object Test {
  def f(implicit x: Int): Int = x * x
  def main(args: Array[String]): Unit = {
    implicit val x: Int = 10
    println(args(0).reflect)
    println(args( 0 ).reflect)
    println(args( 0 /* ignore */).reflect)
    println(f.reflect)
    println((f).reflect)
    println( { f }.reflect)
    println( { f; f }.reflect)
  }
}
