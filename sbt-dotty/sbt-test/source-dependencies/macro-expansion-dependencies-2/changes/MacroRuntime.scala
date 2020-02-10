import scala.quoted._

object MacroRuntime {

   def impl()(using QuoteContext): Expr[Unit] = {
      '{ println("Implementation in Macro") }
   }

}
