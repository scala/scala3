import scala.quoted._
import scala.quoted.autolift._
import scala.tasty.Reflection

import scala.language.implicitConversions

object XmlQuote {

  implicit object SCOps {
    inline def (ctx: => StringContext) xml (args: => (given Scope => Any)*) given Scope: String =
      ${XmlQuote.impl('ctx, 'args, '{implicitly[Scope]})}
  }

  private def impl(receiver: Expr[StringContext], args: Expr[Seq[given Scope => Any]], scope: Expr[Scope]): Expr[String] = '{
    $receiver.s($args.map(_ given $scope.inner): _*)
  }
}

case class Scope(name: String) {
  def inner: Scope = Scope(name + "+")
}

object Scope {
  implicit def topScope: Scope = Scope("top")
}
