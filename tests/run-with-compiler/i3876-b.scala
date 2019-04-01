import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val x: Expr[Int] = '{3}

    val f2: Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }
    println(f2(x).run)
    println(f2(x).show)
  }
}
