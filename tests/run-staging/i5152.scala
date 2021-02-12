import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int])(using Quotes): Expr[Int => Int] = '{identity}

  def peval1()(using Quotes): Expr[Unit] = '{
    lazy val f: Int => Int = ${eval1('{(y: Int) => f(y)})}
  }

  def main(args: Array[String]): Unit = withQuotes {
    val p = peval1()
    println(p.show)
  }

}
