import scala.quoted._

// This test checks the correct interpretation of the inlined value class

object FInterpolation {

  implicit class FInterpolatorHelper(val sc: StringContext) extends AnyVal {
    inline def ff(arg1: Any): String = ${fInterpolation(sc, Seq('arg1))} // error // error
    inline def ff(arg1: Any, arg2: Any): String = ${fInterpolation(sc, Seq('arg1, 'arg2))} // error // error
    inline def ff(arg1: Any, arg2: Any, arg3: Any): String = ${fInterpolation(sc, Seq('arg1, 'arg2, 'arg3))} // error // error
    // ...
  }

  private def liftSeq(using s: Scope)(args: Seq[s.Expr[Any]]): s.Expr[Seq[Any]] = args match {
    case x :: xs  => '{ ($x) +: ${liftSeq(xs)}  }
    case Nil => '{Seq(): Seq[Any]}
  }

  def fInterpolation(using s: Scope)(sc: StringContext, args: Seq[s.Expr[Any]]): s.Expr[String] = {
    val str: s.Expr[String] = Expr(sc.parts.mkString(""))
    val args1: s.Expr[Seq[Any]] = liftSeq(args)
    '{ $str.format($args1: _*) }
  }

  def hello = "hello"

}
