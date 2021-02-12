import scala.quoted.*

def compileImpl[T](expr : Expr[T])(using Quotes) : Expr[T] = {
  import quotes.reflect.*

  def proc(term : Term): Term = {
    term match {
      case Inlined(call, bindings, body) => Inlined(call, bindings, proc(body))
      case l : Literal => l
      case Block(statements, expr) =>
        proc(expr) // FIXME: very wrong
      case s : (Select|Ident) => {
        if( s.symbol.isDefDef ) {
          s.symbol.tree match {
            case DefDef(name, paramss, returnTp, Some(rhs)) => proc(rhs)
          }
        } else {
          ???
        }
      }
    }
  }

  proc(expr.asTerm).asExpr.asInstanceOf[Expr[T]]
}

inline def compile[T](expr : =>T) : T =
  ${ compileImpl('{ expr }) }
