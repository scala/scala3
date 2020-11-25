import scala.quoted._

object MacroRuntime {

   def impl()(using q: Quotes): Expr[Unit] = {
      import quotes.reflect._
      Reporting.error("some error", Position.ofMacroExpansion)
      '{ println("Implementation in MacroCompileError") }
   }

}
