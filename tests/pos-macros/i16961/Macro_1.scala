import scala.quoted.*

inline def myMacro = ${ myMacroImpl }

def myMacroImpl(using Quotes) =
  import quotes.reflect.*

  PolyType(List("arg"))(_ => List(TypeBounds.empty), _ => TypeRepr.of[Any]) match
    case _: TypeLambda => quotes.reflect.report.errorAndAbort("PolyType should not be a TypeLambda")
    case _ => '{ () } // Ok
