import scala.quoted._
object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withNewQuoteContext {
    def f[T](x: Expr[T])(implicit t: Type[T]) = '{
      val z = $x
    }
    println(f('{2})(Type.IntTag).show)
  }
}
