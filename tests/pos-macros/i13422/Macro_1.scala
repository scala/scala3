import scala.quoted._

inline def rule(inline r: Any): Unit = ${ ruleImpl('r) }

def ruleImpl(r: Expr[Any])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  r.asTerm.show
  '{}
}
