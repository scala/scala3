import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T](x: Expr[T])(using t: Type[T]) = '{
      val z: t.Underlying = $x
    }
    println(f('{2})(using Type.of[Int]).show)
  }
}