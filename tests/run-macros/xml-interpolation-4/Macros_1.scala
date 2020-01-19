import scala.quoted._
import scala.quoted.autolift.{given _}

import scala.language.implicitConversions

object XmlQuote {

  implicit object SCOps {
    inline def (ctx: => StringContext) xml (args: => (Scope ?=> Any)*) with Scope : String =
      ${XmlQuote.impl('ctx, 'args, '{implicitly[Scope]})}
  }

  private def impl(receiver: Expr[StringContext], args: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope]) with QuoteContext : Expr[String] = '{
    $receiver.s($args.map(_.with($scope.inner)): _*)
  }
}

case class Scope(name: String) {
  def inner: Scope = Scope(name + "+")
}

object Scope {
  implicit def topScope: Scope = Scope("top")
}
