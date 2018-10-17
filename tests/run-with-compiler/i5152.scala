import scala.quoted._

object Test {
  val tb = Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]): Staged[Int => Int] = '{identity}

  def peval1(): Staged[Unit] = '{
    lazy val f: Int => Int = ${eval1('{(y: Int) => f(y)})}
  }

  def main(args: Array[String]): Unit = {
    println(tb.show(peval1()))
  }

}