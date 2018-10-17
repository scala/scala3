import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def expr: Staged[Int] = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(tb.run(expr))
    println(tb.run(expr))
    println(tb.show(expr))
  }
}
