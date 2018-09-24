import scala.quoted._
import scala.tasty.Tasty

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

// Ideally should be an implicit class but the implicit conversion
// has to be a inline method
class XmlQuote(val ctx: StringContext) {
  inline def xml(args: => Any*): Xml = ~XmlQuote.impl('(this), '(args))
}

object XmlQuote {
  implicit inline def XmlQuote(ctx: StringContext): XmlQuote = new XmlQuote(ctx)

  def impl(receiver: Expr[XmlQuote], args: Expr[Seq[Any]])
          (implicit tasty: Tasty): Expr[Xml] = {
    import tasty._
    import Term._

    def abort(msg: String): Nothing =
      throw new QuoteError(msg)

    // for debugging purpose
    def pp(tree: Tree): Unit = {
      println(tree.show)
      println(tasty.showSourceCode.showTree(tree))
    }

    def liftListOfAny(lst: List[Term]): Expr[List[Any]] = lst match {
      case x :: xs  =>
        val head = x.toExpr[Any]
        val tail = liftListOfAny(xs)
        '{ ~head :: ~tail }
      case Nil => '(Nil)
    }

    def isStringConstant(tree: Term) = tree match {
      case Literal(_) => true
      case _ => false
    }

    // _root_.scala.StringContext.apply([p0, ...]: String*)
    val parts = receiver.toTasty.underlyingArgument match {
      case Apply(
            Select(New(_), _, _),
            List(
              Apply(
                Select(Select(Select(Ident("_root_"), "scala", _), "StringContext", _), "apply", _),
                List(Typed(Repeated(values), _))))) if values.forall(isStringConstant) =>
        values.collect { case Literal(Constant.String(value)) => value }
      case tree =>
        abort(s"String literal expected, but $tree found")
    }

    // [a0, ...]: Any*
    val Typed(Repeated(args0), _) = args.toTasty.underlyingArgument

    val string = parts.mkString("??")
    '(new Xml(~string.toExpr, ~liftListOfAny(args0)))
  }
}
