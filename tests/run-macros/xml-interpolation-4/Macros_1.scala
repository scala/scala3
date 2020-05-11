import scala.quoted._

import scala.language.implicitConversions

object XmlQuote {

  implicit object SCOps {
    extension (ctx: => StringContext) inline def xml (args: => (Scope ?=> Any)*)(using Scope): String =
      ${XmlQuote.impl('ctx, 'args, '{implicitly[Scope]})}
  }

  private def impl(using s: quoted.Scope)(receiver: s.Expr[StringContext], args: s.Expr[Seq[Scope ?=> Any]], scope: s.Expr[Scope]): s.Expr[String] = '{
    $receiver.s($args.map(_(using $scope.inner)): _*)
  }
}

case class Scope(name: String) {
  def inner: Scope = Scope(name + "+")
}

object Scope {
  implicit def topScope: Scope = Scope("top")
}
