import scala.quoted.*

class Macro(using qctx: Quotes): // Anti-pattern: put Quotes in a field
  import qctx.reflect._

  def apply: Expr[Unit] = '{
    println("in quote")
    ${ val a: Term = '{ println("in nested quote") }.asTerm; ??? }
  }
