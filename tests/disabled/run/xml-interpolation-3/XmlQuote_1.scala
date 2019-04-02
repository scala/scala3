import scala.quoted._
import scala.tasty.Tasty
import scala.quoted.autolift._

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit object SCOps {
    inline def xml(this inline ctx: StringContext)(args: => Any*): Xml =
      ${XmlQuote.impl(ctx, 'args)}
  }

  def impl(receiver: StringContext, args: Expr[Seq[Any]]): Expr[Xml] = {
    val string = receiver.parts.mkString("??")
    '{new Xml(${string}, ($args).toList)}
  }
}
