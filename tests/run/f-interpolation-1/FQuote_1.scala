import scala.quoted._
import scala.tasty.Reflection
import scala.quoted.autolift._

import scala.language.implicitConversions

object FQuote {

  implicit class SCOps(ctx: StringContext) {
    inline def ff(args: => Any*): String = ${impl('this, 'args)}
  }

  /*private*/ def impl(receiver: Expr[SCOps], args: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._

    def liftListOfAny(lst: List[Term]): Expr[List[Any]] = lst match {
      case x :: xs  =>
        val head = x.seal[Any]
        val tail = liftListOfAny(xs)
        '{ $head :: $tail }
      case Nil => '{Nil}
    }

    def isStringConstant(tree: Term) = tree match {
      case Term.Literal(_) => true
      case _ => false
    }

    def isSCOpsConversion(tree: Term) =
      tree.symbol.fullName == "FQuote$.SCOps"

    def isStringContextApply(tree: Term) =
      tree.symbol.fullName == "scala.StringContext$.apply"

    // FQuote.SCOps(StringContext.apply([p0, ...]: String*)
    val parts = receiver.unseal.underlyingArgument match {
      case Term.Apply(conv, List(Term.Apply(fun, List(Term.Typed(Term.Repeated(values, _), _)))))
          if isSCOpsConversion(conv) &&
             isStringContextApply(fun) &&
             values.forall(isStringConstant) =>
        values.collect { case Term.Literal(Constant.String(value)) => value }
      case tree =>
        throw new QuoteError(s"String literal expected, but ${tree.show} found")
    }

    // [a0, ...]: Any*
    val Term.Typed(Term.Repeated(allArgs, _), _) = args.unseal.underlyingArgument

    for ((arg, part) <- allArgs.zip(parts.tail)) {
      if (part.startsWith("%d") && !(arg.tpe <:< definitions.IntType)) {
        return '{s"`${${arg.showCode}}` is not of type Int"}
      }

    }

    val string = parts.mkString("")
    '{ new collection.immutable.StringOps(${string}).format($args: _*) }
  }
}
