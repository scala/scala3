import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val expr = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(expr.run)
    println(expr.run)
    println(expr.show)
  }
}
