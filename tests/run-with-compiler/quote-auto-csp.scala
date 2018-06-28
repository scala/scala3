import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = dotty.tools.dotc.quoted.Toolbox.make

  def foo0(n: Int): Expr[Int] = '(n)
  def foo1(n: Int): Expr[Int] = n.toExpr
  def foo2(n: Int): Expr[Int] = '(n + n)
  def foo3(n: Int): Expr[Int] = '(~n.toExpr + ~n.toExpr)
  def foo4: Expr[Int => Expr[Int]] = '(n => '(n))
  def foo5: Expr[Int => Expr[Int]] = '(n => '(n + 3))

  def foo6(n: List[Int])(implicit lift: Liftable[Int]): Expr[List[Int]] = '(n)

  def foo7(n: Int): Expr[Int => Expr[Int]] = '(m => { val n2 = n; '(n2 + m) } )

  def main(args: Array[String]): Unit = {
    println(foo0(3).run)
    println(foo0(3).show)
    println()
    println(foo1(4).run)
    println(foo1(4).show)
    println()
    println(foo2(5).run)
    println(foo2(5).show)
    println()
    println(foo3(6).run)
    println(foo3(6).show)
    println()
    println(foo4.show)
    println(foo4.run.apply(7).run)
    println(foo4.run.apply(7).show)
    println()
    println(foo5.show)
    println(foo5.run.apply(8).run)
    println(foo5.run.apply(8).show)
    println()
    println(foo6(List(1, 2, 3)).run)
    println(foo6(List(1, 2, 3)).show)
    println()
    println(foo7(3).show)
    println(foo7(3).run.apply(9).run)
    println(foo7(3).run.apply(9).show)
  }

  implicit def listIsLiftable: Liftable[List[Int]] = new Liftable[List[Int]] {
    def toExpr(x: List[Int]): Expr[List[Int]] = x match {
      case x :: xs => '{ ~x.toExpr :: ~toExpr(xs) }
      // FIXME `x` is not in ReifyQuotes.levelOf
      // case x :: xs => '{ x :: ~toExpr(xs) }
      case _ => '(Nil: List[Int])
    }
  }

}
