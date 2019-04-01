import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val x: Expr[Int] = '{3}

    val f: Expr[Int => Int] = '{ (x: Int) => x + x }
    println(f(x).run)
    println(f(x).show)
  }
}
