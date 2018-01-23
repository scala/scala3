import scala.quoted._

object FInterpolation {

  implicit class FInterpolatorHelper(val sc: StringContext) extends AnyVal {
    inline def ff(args: Any*): String = ~fInterpolation(sc, '(args))
  }

  def fInterpolation(sc: StringContext, args: Expr[Seq[Any]]): Expr[String] = {
    sc.toString
  }
}
