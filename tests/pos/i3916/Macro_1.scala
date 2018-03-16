import scala.quoted._
import scala.quoted.Liftable._

class FInterpolatorHelper(val sc: StringContext) extends AnyVal {
  inline def ff(arg1: Any): String = ~FInterpolation.fInterpolation(sc, Seq('(arg1)))
  inline def ff(arg1: Any, arg2: Any): String = ~FInterpolation.fInterpolation(sc, Seq('(arg1), '(arg2)))
  inline def ff(arg1: Any, arg2: Any, arg3: Any): String = ~FInterpolation.fInterpolation(sc, Seq('(arg1), '(arg2), '(arg3)))
  // ...
}

object FInterpolation {
  private def liftSeq(args: Seq[Expr[Any]]): Expr[Seq[Any]] = args match {
    case x :: xs  => '{ (~x) +: ~(liftSeq(xs))  }
    case Nil => '(Seq(): Seq[Any])
  }

  def fInterpolation(sc: StringContext, args: Seq[Expr[Any]]): Expr[String] = {
    val str: Expr[String] = sc.parts.mkString("").toExpr
    val args1: Expr[Seq[Any]] = liftSeq(args)
    '{  (~str).format(~args1: _*) }
  }
}
