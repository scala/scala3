trait NameOf:
  transparent inline def nameOf(inline expr: Any): String = ${NameOfImpl.nameOf('expr)}
  transparent inline def nameOf[T](inline expr: T => Any): String = ${NameOfImpl.nameOf('expr)}
object NameOf extends NameOf

import scala.compiletime.*

import scala.annotation.tailrec
import scala.quoted.*

object NameOfImpl {
  def nameOf(expr: Expr[Any])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    @tailrec def extract(tree: Tree): String = tree match {
      case Ident(name) => name
      case Select(_, name) => name
      case Block(List(stmt), term) => extract(stmt)
      case DefDef("$anonfun", _, _, Some(term)) => extract(term)
      case Block(_, term) => extract(term)
      case Apply(term, _) if term.symbol.fullName != "<special-ops>.throw" => extract(term)
      case TypeApply(term, _) => extract(term)
      case Inlined(_, _, term) => extract(term)
      case Typed(term, _) => extract(term)
      case _ => throw new MatchError(s"Unsupported expression: ${expr.show}")
    }
    val name = extract(expr.asTerm)
    Expr(name)
  }
}
