import scala.quoted._

import scala.language.implicitConversions

object FQuote {

  implicit class SCOps(ctx: StringContext) {
    inline def ff(args: => Any*): String = ${impl('this, 'args)}
  }

  /*private*/ def impl(using s: Scope)(receiver: s.Expr[SCOps], args: s.Expr[Seq[Any]]): s.Expr[String] = {
    import s.tasty._

    def liftListOfAny(lst: List[Term]): s.Expr[List[Any]] = lst match {
      case x :: xs  =>
        val head = x.seal
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
    val parts = receiver.underlyingArgument match {
      case Apply(conv, List(Apply(fun, List(Typed(Repeated(values, _), _)))))
          if isSCOpsConversion(conv) &&
             isStringContextApply(fun) &&
             values.forall(isStringConstant) =>
        values.collect { case Literal(Constant(value: String)) => value }
      case tree =>
        report.throwError(s"String literal expected, but ${tree.showExtractors} found")
    }

    // [a0, ...]: Any*
    val Typed(Repeated(allArgs, _), _) = args.underlyingArgument

    for ((arg, part) <- allArgs.zip(parts.tail)) {
      if (part.startsWith("%d") && !(arg.tpe <:< Type.of[Int])) {
        return '{s"`${${Expr(arg.show)}}` is not of type Int"}
      }

    }

    val string = parts.mkString("")
    '{ new collection.immutable.StringOps(${Expr(string)}).format($args: _*) }
  }
}
