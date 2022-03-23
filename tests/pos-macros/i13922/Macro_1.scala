import scala.quoted.*

inline def optPrettyPrinter[T]: Option[T] => Option[T] =
  ${ optPrettyPrinterImpl[T] }

private def optPrettyPrinterImpl[T: Type](using Quotes): Expr[Option[T] => Option[T]] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[T]

  val fn = Lambda(
    Symbol.spliceOwner,
    MethodType(List("macroVal"))(
      _ => List(tpe),
      _ => tpe
    ),
    {
      case (m, List(arg: Term)) =>
        given Quotes = m.asQuotes
        ValDef.let(m, "v", arg) { v =>
          '{
            val vv = ${ v.asExprOf[T] }
            println("v=" + vv.toString())
            vv
          }.asTerm
        }

      case _ =>
        report.errorAndAbort("Fails compile")
    }
  ).asExprOf[T => T]

  '{ (_: Option[T]).map(${ fn }) }
}
