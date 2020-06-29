import scala.quoted._
import scala.quoted.autolift

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
    def (ctx: scala.StringContext).xml: StringContext = ctx
  }

  inline def (inline ctx: XMLOps.StringContext).apply(inline args: Any*): Xml =
    ${XmlQuote.impl('ctx, 'args)}
  // inline def (inline ctx: SCOps.StringContext).unapplySeq(...): Xml = ...


  def impl(receiver: Expr[XMLOps.StringContext], args: Expr[Seq[Any]])(using QuoteContext): Expr[Xml] = {
    val string = receiver match {
      case '{ XMLOps.extension_xml(${Unlifted(sc)}) } => sc.parts.mkString("??")
    }
    '{new Xml(${string}, $args.toList)}
  }
}
