
import dotty.tools.dotc.quoted.Runners._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {

    val x: Expr[Int] = 5

    println(fInterpolation(
      new StringContext("abc", "xyz"),
      Seq(x.asInstanceOf[Expr[Any]])
    ).show)

  }

  def fInterpolation(sc: StringContext, args: Seq[Expr[Any]]): Expr[String] = {
    assert(sc.parts.size - 1 == args.size)
    sc.toString
  }

}
