import scala.quoted._

object MacroRuntime {

   def impl()(given qctx: QuoteContext): Expr[Unit] = {
      import qctx.tasty._
      error("some error", rootPosition)
      '{ println("Implementation in MacroCompileError") }
   }

}
