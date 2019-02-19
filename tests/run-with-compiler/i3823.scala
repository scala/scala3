import scala.quoted.Toolbox.Default._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T: Type](x: Expr[T])(t: Type[T]) = '{
      val z: $t = $x
    }
    println(f('{2})('[Int]).show)
  }
}