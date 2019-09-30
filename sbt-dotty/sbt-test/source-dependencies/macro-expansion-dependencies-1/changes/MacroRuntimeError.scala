import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(given qctx: QuoteContext): Expr[Unit] = {
      '{ ??? }
   }

}
