import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

object XmlQuote {

  implicit class SCOps(ctx: StringContext) {
    inline def xml(args: => Any*): Xml = ${XmlQuote.impl('this, 'args)}
  }

  def impl(receiver: Expr[SCOps], args: Expr[Seq[Any]])
          (implicit reflect: Reflection): Expr[Xml] = {
    import reflect._

    def abort(msg: String): Nothing =
      QuoteError(msg)

    // for debugging purpose
    def pp(tree: Tree): Unit = {
      println(tree.showExtractors)
      println(tree.show)
    }

    def liftListOfAny(lst: List[Term]): Expr[List[Any]] = lst match {
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
      tree.symbol.fullName == "XmlQuote$.SCOps"

    def isStringContextApply(tree: Term) =
      tree.symbol.fullName == "scala.StringContext$.apply"

    // XmlQuote.SCOps(StringContext.apply([p0, ...]: String*)
    val parts = receiver.unseal.underlyingArgument match {
      case Apply(conv, List(Apply(fun, List(Typed(Repeated(values, _), _)))))
          if isSCOpsConversion(conv) &&
             isStringContextApply(fun) &&
             values.forall(isStringConstant) =>
        values.collect { case Literal(Constant.String(value)) => value }
      case tree =>
        abort(s"String literal expected, but ${tree.showExtractors} found")
    }

    // [a0, ...]: Any*
    val Typed(Repeated(args0, _), _) = args.unseal.underlyingArgument

    val string = parts.mkString("??")
    '{new Xml(${string}, ${liftListOfAny(args0)})}
  }
}
