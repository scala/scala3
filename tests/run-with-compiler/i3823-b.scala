import scala.quoted._
import scala.quoted.staging._
object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T](x: Expr[T])(implicit t: Type[T]) = '{
      val z: $t = $x
    }
    println(f('{2})(Type.IntTag).show)
  }
}
