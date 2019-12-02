import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(given QuoteContext): Expr[Unit] =
      MacroRuntime.impl()


}
