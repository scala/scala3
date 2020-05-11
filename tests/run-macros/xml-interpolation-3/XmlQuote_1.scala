import scala.quoted._

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit object SCOps {
    extension (inline ctx: StringContext) inline def xml (args: => Any*): Xml =
      ${XmlQuote.impl('ctx, 'args)}
  }

  def impl(using s: Scope)(receiver: s.Expr[StringContext], args: s.Expr[Seq[Any]]): s.Expr[Xml] = {
    val string = receiver.unliftOrError.parts.mkString("??")
    '{new Xml(${Expr(string)}, $args.toList)}
  }
}
