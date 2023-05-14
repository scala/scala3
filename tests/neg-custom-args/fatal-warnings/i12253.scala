import scala.quoted.{given, *}
import deriving.*,  compiletime.*

object MacroUtils:
  transparent inline def extractNameFromSelector[To, T](inline code: To => T) = ${extractNameFromSelectorImpl('code)}

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using q1: Quotes): Expr[String] =
    import quotes.reflect.*
    val extractors = new Extractors
    code.asTerm match
     case extractors.InlinedLambda(_, Select(_, name)) => Expr(name) // error // error
     case t => report.throwError(s"Illegal argument to extractor: ${code.show}, in tasty: $t")

  class Extractors(using val q2: Quotes):
    //attempt to strip away consecutive inlines in AST and extract only final lambda
    import quotes.reflect.*

    object InlinedLambda:
      def unapply(arg: Term): Option[(List[ValDef], Term)] =
        arg match
          case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
          case Inlined(_, _, nested) => InlinedLambda.unapply(nested)
          case t => None
    end InlinedLambda

  end Extractors
end MacroUtils
// nopos-error
