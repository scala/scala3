import dotty.tools.dotc.quoted.Runners._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    val u: Expr[Unit] = ()
    println(u.show)
    println(u.run)
  }
}
