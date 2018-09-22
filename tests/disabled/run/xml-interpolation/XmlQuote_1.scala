import scala.quoted._
import scala.tasty.Tasty

import scala.language.implicitConversions

case class Xml(parts: String, args: List[Any])

// Ideally should be an implicit class but the implicit conversion
// has to be a inline method
class XmlQuote(ctx: => StringContext) {
  inline def xml(args: => Any*): Xml = ~XmlQuote.impl('(ctx), '(args))
}

object XmlQuote {
  implicit inline def XmlQuote(ctx: => StringContext): XmlQuote = new XmlQuote(ctx)

  def impl(ctx: Expr[StringContext], args: Expr[Seq[Any]])
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
    val parts = ctx.toTasty match {
      case Inlined(_, _,
        Apply(
          Select(Select(Select(Ident("_root_"), "scala", _), "StringContext", _), "apply", _),
          List(Typed(Repeated(values), _)))) if values.forall(isStringConstant) =>
        values.collect { case Literal(Constant.String(value)) => value }
      case tree =>
        abort(s"String literal expected, but $tree found")
    }

    // [a0, ...]: Any*
    val Inlined(_, _, Typed(Repeated(args0), _)) = args.toTasty

    val string = parts.mkString("??")
    '(new Xml(~string.toExpr, ~liftListOfAny(args0)))
  }
}
