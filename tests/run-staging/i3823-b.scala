import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    def f[T](x: Expr[T])(implicit t: Type[T]) = '{
      val z: t.Underlying = $x
    }
    println(f('{2})(Type.of[Int]).show)
  }
}
