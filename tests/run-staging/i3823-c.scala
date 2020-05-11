import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    def f[T](x: scope.Expr[T])(implicit t: scope.Type[T]) = '{
      val z = $x
    }
    println(f('{2})('[Int]).show)
  }
}
