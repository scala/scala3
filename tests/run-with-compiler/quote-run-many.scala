import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

    def expr(i: Int) = '{
      val a = 3 + ${i.toExpr}
      2 + a
    }
    for (i <- 0 to 200)
      expr(i).run
  }
}
