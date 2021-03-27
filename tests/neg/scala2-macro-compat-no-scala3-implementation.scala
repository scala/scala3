object Macro1 {
  import scala.language.experimental.macros
  def lineNumber: Int = macro LineNumberMacro2.thisLineNumberImpl // error: No Scala 3 implementation found for this Scala 2 macro.
}

object LineNumberMacro2 {
  class Context: // Dummy scala.reflect.macros.Context
    type Expr[+T]

  def thisLineNumberImpl(context: Context): context.Expr[Int] = {
    // val lineNumber = context.enclosingPosition.line
    // context.literal(lineNumber)
    ???
  }
}
