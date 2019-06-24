import scala.quoted._

import scala.tasty._

object Test {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = {
    '[List]
    val list = bound('{List(1, 2, 3)})
    println(run(list.show.toExpr))
    println(run(list))

    val opt = bound('{Option(4)})
    println(run(opt.show.toExpr))
    println(run(opt))

    val map = bound('{Map(4 -> 1)})
    println(run(map.show.toExpr))
    println(run(map))
  }

  def bound[T: Type, S[_]: Type](x: Expr[S[T]]): Expr[S[T]] = '{
    val y: S[T] = $x
    y
  }
}
