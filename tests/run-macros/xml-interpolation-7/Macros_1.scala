import scala.quoted.*

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  // Encoding for
  //
  // implicit class SCOps(s: StringContext) {
  //   object xml {
  //     def apply(exprs: Any*) = ...
  //     def unapplySeq(...) = ...
  //   }
  // }
  object XMLOps {
    opaque type StringContext = scala.StringContext
    extension (ctx: scala.StringContext) def xml: StringContext = ctx
  }

  extension (inline ctx: XMLOps.StringContext) inline def apply(inline args: Any*): Xml =
    ${XmlQuote.impl('ctx, 'args)}
  // extension (inline ctx: SCOps.StringContext) inline def unapplySeq(...): Xml = ...


  def impl(receiver: Expr[XMLOps.StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[Xml] = {
    val string = receiver match {
      case '{ XMLOps.xml(${Expr(sc)}) } => sc.parts.mkString("??")
    }
    '{new Xml(${Expr(string)}, $args.toList)}
  }
}
