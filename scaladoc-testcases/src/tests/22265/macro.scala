import scala.quoted._

object TestBuilder:
  // transparent is needed
  transparent inline def apply(inline expr: Unit): Any =
    ${ TestBuilder.processTests('expr) }

  def processTests(using Quotes)(body: Expr[Unit]): Expr[Any] =
    import quotes.reflect._
    body.asTerm match {
      case Inlined(_, _, bindings) =>
        '{ ${bindings.asExpr}; () } // can also be List(${bindings})
    }
