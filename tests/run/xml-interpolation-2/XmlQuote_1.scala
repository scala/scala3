import scala.quoted._

import scala.tasty.Tasty

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  class SCOps(ctx: => StringContext) {
    inline def xml(args: Any*): Xml = ~XmlQuote.impl('(this), '(args))
  }
  implicit inline def SCOps(ctx: => StringContext): SCOps = new SCOps(ctx)

  def impl(receiver: Expr[SCOps], args: Expr[Seq[Any]])
          (implicit tasty: Tasty): Expr[Xml] = {
    import tasty._
    import Term._

    // for debugging purpose
    def pp(tree: Tree): Unit = {
      println(tree.show)
      println(tasty.showSourceCode.showTree(tree))
    }

    def isSCOpsConversion(tree: Term) =
      tree.symbol.fullName == "XmlQuote$.SCOps" ||
      tree.symbol.fullName == "XmlQuote$.SCOps.<init>"

    def isStringContextApply(tree: Term) =
      tree.symbol.fullName == "scala.StringContext$.apply" ||
      tree.symbol.fullName == "scala.StringContext.<init>"

    // XmlQuote.SCOps(StringContext.apply([p0, ...]: String*)
    val parts: List[String] = receiver.toTasty.underlying match {
      case Apply(conv, List(ctx1)) if isSCOpsConversion(conv) =>
        ctx1 match {
          case Apply(fun, List(Typed(Repeated(values), _))) if isStringContextApply(fun) =>
            values.iterator.map {
              case Literal(Constant.String(value)) => value
              case _ => QuoteError("Expected statically known String")
            }.toList
          case _ => QuoteError("Expected statically known StringContext")
        }
      case _ =>
        QuoteError("Expected statically known SCOps")
    }

    // [a0, ...]: Any*
    val args2: Expr[List[Any]] = args.toTasty.underlyingArgument match {
      case Typed(Repeated(args0), _) => // statically known args, make list directly
        def liftListOfAny(lst: List[Expr[Any]]): Expr[List[Any]] = lst match {
          case x :: xs  => '{ ~x :: ~liftListOfAny(xs) }
          case Nil => '(Nil)
        }
        liftListOfAny(args0.map(_.toExpr[Any]))
      case _ =>
        '((~args).toList)

    }

    val string = parts.mkString("??")
    '(new Xml(~string.toExpr, ~args2))
  }
}
