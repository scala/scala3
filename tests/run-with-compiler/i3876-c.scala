import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit def toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val x: Expr[Int] = '{3}

    val f3: Expr[Int => Int] = '{
      val f: (x: Int) => Int = x => x + x
      f
    }
    println(f3(x).run)
    println(f3(x).show) // TODO improve printer
  }
}
