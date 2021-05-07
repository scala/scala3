
import scala.quoted.*
import deriving.*,  compiletime.*

object MacroUtils:
  transparent inline def extractNameFromSelector[To, T](inline code: To => T) = ${extractNameFromSelectorImpl('code)}

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using Quotes): Expr[String] =
    import quotes.reflect.*
    code.asTerm match
      case InlinedLambda(_, Select(_, name)) => Expr(name)
      case t => Expr("")

  object InlinedLambda:
    def unapply(using Quotes)(arg: quotes.reflect.Term): Option[(List[quotes.reflect.ValDef], quotes.reflect.Term)] =
      import quotes.reflect.*
      arg match
        case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
        case Inlined(_, _, nested) => InlinedLambda.unapply(nested)
        case t => None
  end InlinedLambda

end MacroUtils

