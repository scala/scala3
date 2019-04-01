import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]): Expr[Int => Int] = '{identity}

  def peval1(): Expr[Unit] = '{
    lazy val f: Int => Int = ${eval1('{(y: Int) => f(y)})}
  }

  def main(args: Array[String]): Unit = {
    val p = peval1()
    println(p.show)
  }

}