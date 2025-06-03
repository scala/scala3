// https://github.com/scalacenter/scala-debug-adapter/issues/425
class A:
  val a1 = "a1"
  var a2 = 1
  def m = "m"
  class D:
    override def toString: String = "D"
  object D:
    override def toString: String = "D$"

object Test:
  private class B extends A

  def main(args: Array[String]): Unit =
    val b = new B
    println("foo")
