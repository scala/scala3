import scala.quoted.*

import scala.language.implicitConversions

object XmlQuote {

  implicit object SCOps {
    extension (ctx: => StringContext) inline def xml (args: => (Scope ?=> Any)*)(using Scope): String =
      ${XmlQuote.impl('ctx, 'args, '{implicitly[Scope]})}
  }

  private def impl(receiver: Expr[StringContext], args: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using Quotes): Expr[String] = '{
    $receiver.s($args.map(_(using $scope.inner))*)
  }
}

case class Scope(name: String) {
  def inner: Scope = Scope(name + "+")
}

object Scope {
  implicit def topScope: Scope = Scope("top")
}
