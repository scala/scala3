

object Macro1 {
  import scala.language.experimental.macros
  def lineNumber: Int = macro LineNumberMacro2.thisLineNumberImpl
  inline def lineNumber: Int = 4
}

object Macro1Consume {
  val test = Macro1.lineNumber
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

