import scala.quoted._

object MacroRuntime {

   def impl()(using Quotes): Expr[Unit] = {
      '{ println("Implementation in Macro") }
   }

}
