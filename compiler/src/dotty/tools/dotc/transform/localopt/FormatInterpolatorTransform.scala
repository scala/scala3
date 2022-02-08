package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*

object FormatInterpolatorTransform:

  /** For f"${arg}%xpart", check format conversions and return (format, args)
   *  suitable for String.format(format, args).
   */
  def checked(fun: Tree, args0: Tree)(using Context): (Tree, Tree) =
    val (partsExpr, parts) = fun match
      case TypeApply(Select(Apply(_, (parts: SeqLiteral) :: Nil), _), _) =>
        (parts.elems, parts.elems.map { case Literal(Constant(s: String)) => s })
      case _ =>
        report.error("Expected statically known StringContext", fun.srcPos)
        (Nil, Nil)
    val (args, elemtpt) = args0 match
      case seqlit: SeqLiteral => (seqlit.elems, seqlit.elemtpt)
      case _ =>
        report.error("Expected statically known argument list", args0.srcPos)
        (Nil, EmptyTree)

    def literally(s: String) = Literal(Constant(s))
    if parts.lengthIs != args.length + 1 then
      val badParts =
        if parts.isEmpty then "there are no parts"
        else s"too ${if parts.lengthIs > args.length + 1 then "few" else "many"} arguments for interpolated string"
      report.error(badParts, fun.srcPos)
      (literally(""), args0)
    else
      val checker = TypedFormatChecker(partsExpr, parts, args)
      val (format, formatArgs) = checker.checked
      if format.isEmpty then (literally(parts.mkString), args0)
      else (literally(format.mkString), SeqLiteral(formatArgs.toList, elemtpt))
  end checked
end FormatInterpolatorTransform
