import scala.quoted.*

object InfoMacro {
  inline def reportInfo(msg: String): Unit = ${ reportInfoMacro('msg) }

  def reportInfoMacro(msg: Expr[String])(using Quotes): Expr[Unit] = {
    import quotes.reflect.report

    // Report an info diagnostic
    report.info(s"Info from macro: ${msg.valueOrAbort}")

    '{ () }
  }
}
