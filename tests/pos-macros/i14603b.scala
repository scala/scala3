import scala.language.experimental.macros
import scala.quoted.*

class Runtime

class Macro(using qctx: Quotes) { // Anti-pattern: put Quotes in a field
  import qctx.reflect._

  def apply[A: Type](x: Expr[A]): Expr[Unit] = {
    '{
      val rt: Runtime = ???
      ${Block(doExprs('{ rt }.asTerm), '{ () }.asTerm).asExprOf[Unit]}
    }
  }

  private def doExprs(rt: Term): List[Term] = Nil
}
