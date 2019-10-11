import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T](x: Expr[T])(implicit t: TypeTag[T]) = '{
      val z = $x
    }
    println(f('{2})(TypeTag.IntTag).show)
  }
}
