import scala.quoted._

// This test checks the correct interpretation of the inlined value class

object FInterpolation {

  implicit class FInterpolatorHelper(val sc: StringContext) extends AnyVal {
    inline def ff(arg1: Any): String = ~fInterpolation(sc, Seq('(arg1)))
    inline def ff(arg1: Any, arg2: Any): String = ~fInterpolation(sc, Seq('(arg1), '(arg2)))
    inline def ff(arg1: Any, arg2: Any, arg3: Any): String = ~fInterpolation(sc, Seq('(arg1), '(arg2), '(arg3)))
    // ...
  }

  private def liftSeq(args: Seq[Expr[Any]]): Expr[Seq[Any]] = args match {
    case x :: xs  => '{ (~x) +: ~(liftSeq(xs))  }
    case Nil => '(Seq(): Seq[Any])
  }

  def fInterpolation(sc: StringContext, args: Seq[Expr[Any]]): Expr[String] = {
    val str: Expr[String] = sc.parts.mkString("").toExpr
    val args1: Expr[Seq[Any]] = liftSeq(args)
    '{  (~str).format(~args1: _*) }
  }

  def hello = "hello"

}