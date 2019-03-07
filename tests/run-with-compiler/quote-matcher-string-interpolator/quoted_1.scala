
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.runtime.quoted.Matcher.{Hole, hole, bindHole}

object Macros {

  inline def (self: => StringContext) xyz(args: => String*): String = ${impl('self, 'args)}

  private def impl(self: Expr[StringContext], args: Expr[Seq[String]])(implicit reflect: Reflection): Expr[String] = {
    object StringContextExpr extends ExprMatch[Tuple1[Expr[Seq[String]]]]('{StringContext(hole[Seq[String]]: _*)}) // case '{ StringContext($args: _*) } =>

    self match {
      // case '{ StringContext(${parts: _*}) } =>
      // case '{ StringContext(${Repeated(parts)}: _*) } =>
      // scala.runtime.quoted.Matcher.unapplySeq[Tuple1[Expr[Seq[String]]]](Tuple1(Repeated(parts: Seq[Expr[String]]): Expr[Seq[String]]))('{ StringContext(hole[Seq[String]]: _*) }, reflect)
      case StringContextExpr(Tuple1(Repeated(parts))) =>
        val parts2 = parts.map(x => '{ $x.reverse }).toList.toExprOfList
        '{ StringContext($parts2: _*).s($args: _*) }
      case _ =>
        '{ "ERROR" }
    }

  }

}

//
// Helper to abstract call to scala.runtime.quoted.Matcher.unapplySeq and setup an object with the unapply
//

class ExprMatch[Tup <: Tuple](pattern: Expr[_]) {
  def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tup] =
    scala.runtime.quoted.Matcher.unapply(x)(pattern, reflect)
}
