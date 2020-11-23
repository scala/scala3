import scala.quoted._

object MacroRuntime {

   def impl()(using Quotes): Expr[Unit] = {
      import qctx.reflect._
      error("some error", rootPosition)
      '{ println("Implementation in MacroCompileError") }
   }

}
