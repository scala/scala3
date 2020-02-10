import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(using QuoteContext): Expr[Unit] =
      MacroRuntime.impl()


}
