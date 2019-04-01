import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

    val u: Expr[Unit] = '{}
    println(u.show)
    println(u.run)
  }
}
