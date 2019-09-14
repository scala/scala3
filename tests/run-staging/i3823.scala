import scala.quoted._
import scala.quoted.staging._
object Test {
  given as Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T: Type](x: Expr[T])(t: Type[T]) = '{
      val z: $t = $x
    }
    println(f('{2})('[Int]).show)
  }
}