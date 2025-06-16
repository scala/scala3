class A
trait B:
  def m: String = "m"

object Test:
  def main(args: Array[String]): Unit =
    val c: A & B = new A with B {}
    println(c.m)
