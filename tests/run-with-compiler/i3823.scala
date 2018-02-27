import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T](x: Expr[T])(t: Type[T]) = '{
      val z: t.unary_~ = ~x
    }
    println(f('(2))('[Int]).show)
  }
}