import scala.quoted._
import scala.quoted.autolift.{given _}

import scala.language.implicitConversions

object XmlQuote {

  implicit object SCOps {
    inline def (ctx: => StringContext) xml (args: => (Scope ?=> Any)*)(using Scope): String =
      ${XmlQuote.impl('ctx, 'args, '{implicitly[Scope]})}
  }

  private def impl(receiver: Expr[StringContext], args: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using QuoteContext): Expr[String] = '{
    $receiver.s($args.map(_(using $scope.inner)): _*)
  }
}

case class Scope(name: String) {
  def inner: Scope = Scope(name + "+")
}

object Scope {
  implicit def topScope: Scope = Scope("top")
}
