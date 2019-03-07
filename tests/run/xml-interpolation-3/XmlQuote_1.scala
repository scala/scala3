import scala.quoted._
import scala.quoted.autolift._
import scala.tasty.Reflection

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit object SCOps {
    inline def (inline ctx: StringContext) xml (args: => Any*): Xml =
      ${XmlQuote.impl(ctx, 'args)}
  }

  def impl(receiver: StringContext, args: Expr[Seq[Any]]): Expr[Xml] = {
    val string = receiver.parts.mkString("??")
    '{new Xml(${string}, $args.toList)}
  }
}
