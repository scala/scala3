import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(using qctx: QuoteContext): Expr[Unit] = {
      import qctx.reflect._
      error("some error", rootPosition)
      '{ println("Implementation in MacroCompileError") }
   }

}
