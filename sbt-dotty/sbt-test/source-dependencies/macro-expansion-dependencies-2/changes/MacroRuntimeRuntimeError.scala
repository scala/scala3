import scala.quoted._

object MacroRuntime {

   def impl()(using qctx: QuoteContext): Expr[Unit] = {
      '{ ??? }
   }

}
