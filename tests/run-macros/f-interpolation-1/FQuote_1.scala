import scala.quoted.*

import scala.language.implicitConversions

object FQuote {

  implicit class SCOps(ctx: StringContext) {
    inline def ff(args: => Any*): String = ${impl('this, 'args)}
  }

  /*private*/ def impl(receiver: Expr[SCOps], args: Expr[Seq[Any]])(using Quotes) : Expr[String] = {
    import quotes.reflect.*

    def liftListOfAny(lst: List[Term]): Expr[List[Any]] = lst match {
      case x :: xs  =>
        val head = x.asExpr
        val tail = liftListOfAny(xs)
        '{ $head :: $tail }
      case Nil => '{Nil}
    }

    def isStringConstant(tree: Term) = tree match {
      case Literal(_) => true
      case _ => false
    }

    def isSCOpsConversion(tree: Term) =
      tree.symbol.fullName == "FQuote$.SCOps"

    def isStringContextApply(tree: Term) =
      tree.symbol.fullName == "scala.StringContext$.apply"

    // FQuote.SCOps(StringContext.apply([p0, ...]: String*)
    val parts = receiver.asTerm.underlyingArgument match {
      case Apply(conv, List(Apply(fun, List(Typed(Repeated(values, _), _)))))
          if isSCOpsConversion(conv) &&
             isStringContextApply(fun) &&
             values.forall(isStringConstant) =>
        values.collect { case Literal(StringConstant(value)) => value }
      case tree =>
        report.error(s"String literal expected, but ${tree.show(using Printer.TreeStructure)} found")
        return '{???}
    }

    // [a0, ...]: Any*
    val Typed(Repeated(allArgs, _), _) = args.asTerm.underlyingArgument

    for ((arg, part) <- allArgs.zip(parts.tail)) {
      if (part.startsWith("%d") && !(arg.tpe <:< TypeRepr.of[Int])) {
        return '{s"`${${Expr(arg.show)}}` is not of type Int"}
      }

    }

    val string = parts.mkString("")
    '{ new collection.immutable.StringOps(${Expr(string)}).format($args*) }
  }
}
