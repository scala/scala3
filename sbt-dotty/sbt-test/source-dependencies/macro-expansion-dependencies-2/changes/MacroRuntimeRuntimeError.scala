import scala.quoted._

object MacroRuntime {

   def impl()(using q: Quotes): Expr[Unit] = {
      '{ ??? }
   }

}
