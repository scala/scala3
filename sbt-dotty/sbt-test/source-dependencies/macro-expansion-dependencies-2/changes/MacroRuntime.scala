import scala.quoted._

object MacroRuntime {

   def impl()(given QuoteContext): Expr[Unit] = {
      '{ println("Implementation in Macro") }
   }

}
