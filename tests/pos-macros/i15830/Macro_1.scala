// https://github.com/scala/scala3/issues/15830
import scala.quoted.*

object macros:

  inline def assertCondition(inline cond: Boolean, inline message: String): Unit =
    ${ assertConditionImpl('cond, 'message) }

  private def assertConditionImpl(cond: Expr[Boolean], message: Expr[String])(using Quotes): Expr[Unit] =
    val report = quotes.reflect.report
    val condValue = cond.valueOrAbort
    val messageValue = message.value.getOrElse("<Unknown message>")
    if !condValue then report.errorAndAbort(messageValue)
    else '{()}

end macros
