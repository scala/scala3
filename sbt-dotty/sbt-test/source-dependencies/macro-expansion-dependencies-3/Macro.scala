import scala.quoted._

object Macro {

   inline def f(): Unit = ${ macroImplementation }

   def macroImplementation(given qctx: QuoteContext): Expr[Unit] = {
      val clazz = Class.forName("MacroRuntime")
      val method = clazz.getMethod("impl", classOf[QuoteContext])
      method.invoke(null, qctx).asInstanceOf[Expr[Unit]]
   }


}
