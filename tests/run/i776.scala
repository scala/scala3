class A1(val s: Int)
class A2(s: Int) extends A1(s)

object Test {
  def main(args: Array[String]): Unit = new A2(2).s
}
