object Macro1 {
  import scala.language.experimental.macros
  def lineNumber: Int = macro LineNumberMacro.thisLineNumberImpl
  inline def lineNumber: Int = ${ LineNumberMacro.thisLineNumberExpr }
}

object LineNumberMacro {
  import scala.reflect.macros._
  def thisLineNumberImpl(context: Context): context.Expr[Int] = {
    val lineNumber = context.enclosingPosition.line
    context.literal(lineNumber)
  }

  import scala.quoted._
  def thisLineNumberExpr(using qctx: QuoteContext): Expr[Int] = {
    // import qctx.tasty.{_, given _}
    // Expr(rootPosition.startLine + 1)
    Expr(3)
  }
}
