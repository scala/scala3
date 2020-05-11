import scala.quoted._
import scala.quoted.staging._
object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    def f[T](using s: Scope)(x: s.Expr[T])(using t: s.Type[T]) = '{
      val z: $t = $x
    }
    println(f('{2})(using '[Int]).show)
  }
}