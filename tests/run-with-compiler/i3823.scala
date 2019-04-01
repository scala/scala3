import scala.quoted._
object Test {
  def main(args: Array[String]): Unit = {
    def f[T: Type](x: Expr[T])(t: Type[T]) = '{
      val z: $t = $x
    }
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    println(f('{2})('[Int]).show)
  }
}