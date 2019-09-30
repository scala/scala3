import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(given qctx: QuoteContext): Expr[Unit] = {
      import qctx.tasty._
      error("some error", rootPosition)
      '{ println("Implementation in MacroCompileError") }
   }

}
