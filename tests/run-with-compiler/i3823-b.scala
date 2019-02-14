import scala.quoted.Toolbox.Default._
import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T](x: Expr[T])(implicit t: Type[T]) = '{
      val z: t.unary_~ = ~x
    }
    println(f('(2))(Type.IntTag).show)
  }
}