import scala.quoted._

object MacroRuntime {

   def impl()(given qctx: QuoteContext): Expr[Unit] = {
      '{ ??? }
   }

}
