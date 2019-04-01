import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

    val q = f
    println(q.run)
    println(q.show)
  }

  def f: Expr[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g: Expr[Int] = '{
    val a = 9
    a + 0
  }
}
