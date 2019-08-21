import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]) given QuoteContext: Expr[Int => Int] = '{identity}

  def peval1()given QuoteContext: Expr[Unit] = '{
    lazy val f: Int => Int = ${eval1('{(y: Int) => f(y)})}
  }

  def main(args: Array[String]): Unit = withQuoteContext {
    val p = peval1()
    println(p.show)
  }

}
