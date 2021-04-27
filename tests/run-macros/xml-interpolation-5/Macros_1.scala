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
  object SCOps {
    opaque type StringContext = scala.StringContext
    def apply(sc: scala.StringContext): StringContext = sc
  }
  extension (inline ctx: StringContext) transparent inline def xml: SCOps.StringContext = SCOps(ctx)
  extension (inline ctx: SCOps.StringContext) inline def apply(inline args: Any*): Xml =
    ${XmlQuote.impl('ctx, 'args)}
  // extension (inline ctx: SCOps.StringContext) inline def unapplySeq(...): Xml = ...


  def impl(receiver: Expr[SCOps.StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[Xml] = {
    val string = receiver match {
      case '{ SCOps(${Expr(sc)}) } => Expr(sc.parts.mkString("??"))
    }
    '{new Xml(${string}, $args.toList)}
  }
}
