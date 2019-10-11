import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T: TypeTag](x: Expr[T])(t: TypeTag[T]) = '{
      val z: $t = $x
    }
    println(f('{2})('[Int]).show)
  }
}
