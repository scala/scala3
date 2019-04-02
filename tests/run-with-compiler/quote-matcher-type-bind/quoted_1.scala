
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.runtime.quoted.Matcher._

object Macros {

  inline def swapFandG(x: => Unit): Unit = ${impl('x)}

  private def impl(x: Expr[Unit])(implicit reflect: Reflection): Expr[Unit] = {

    type TT // type binding
    object DSLf extends ExprMatch[Tuple2[Type[TT], Expr[TT]]]('{ type Binding$T1;  DSL.f[Hole[Binding$T1]](hole[Binding$T1]) }) // case '{ DSL.f[$t]($x) } =>

    x match {
      // case '{ DSL.f[$t]($x) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Type[tt /*type binding*/], Expr[tt]]](Tuple2(t, x)(/*implicits*/ '{ DSL.f[Hole[Nothing, Any]](hole[Any])] }, reflect) if consforms(x, t) =>
      case DSLf(t, x) =>
        implicit val tt = t
        '{ DSL.g[$t]($x) }

      // case '{ DSL.f[$t]($x) } =>
//      case DSLg(t, x) =>
//        '{ DSL.f[$t]($x) }

      case _ =>
        x
    }
  }

}

//
// DSL in which the user write the code
//

object DSL {
  def f[T](x: T): Unit = println("f: " + x.toString)
  def g[T](x: T): Unit = println("g: " + x.toString)
}

//
// Helper to abstract call to scala.runtime.quoted.Matcher.unapply and setup an object with the unapply
//

class ExprMatch[Tup <: Tuple](pattern: Expr[_]) {
  def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tup] =
    scala.runtime.quoted.Matcher.unapply[Tup](x)(pattern, reflect)
}
