import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]): Expr[Int] = '{$ff(42)}

  def peval1(): Expr[Unit] = '{
    def f(x: Int): Int = ${eval1('f)}
  }

  def main(args: Array[String]): Unit = {
    val p = peval1()
    println(p.show)
  }

}