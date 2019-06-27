import scala.quoted._
object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    def f[T: Type](x: Expr[T])(t: Type[T]) = '{
      val z: $t = $x
    }
    println(f('{2})('[Int]).show)
  }
}