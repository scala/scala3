import scala.quoted._

object FInterpolation {

  implicit class FInterpolatorHelper(val sc: StringContext) extends AnyVal {
    inline def ff(arg1: Any): String = ~fInterpolation(sc, Seq('(arg1)))
    inline def ff(arg1: Any, arg2: Any): String = ~fInterpolation(sc, Seq('(arg1), '(arg2)))
    inline def ff(arg1: Any, arg2: Any, arg3: Any): String = ~fInterpolation(sc, Seq('(arg1), '(arg2), '(arg3)))
    // ...
  }

  def fInterpolation(sc: StringContext, args: Seq[Expr[Any]]): Expr[String] = {
    sc.toString
  }
}
