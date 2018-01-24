
import dotty.tools.dotc.quoted.Runners._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {

    val x: Expr[Int] = 5

    println(fInterpolation(
      new StringContext("abc", "xyz"),
      '(Seq[Any](~x))
    ).show)

  }

  def fInterpolation(sc: StringContext, args: Expr[Seq[Any]]): Expr[String] = {
    assert(sc.parts.size - 1 == args.run.size)
    sc.toString
  }

}
