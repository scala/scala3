import scala.quoted.*

class Bar

inline def foo: Unit = ${ fooExpr }

def fooExpr(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  Implicits.search(TypeRepr.of[Bar]) match
    case res: ImplicitSearchSuccess   => '{}
    case failure: ImplicitSearchFailure =>
      report.errorAndAbort(failure.explanation)


inline given bar: Bar = ${ barExpr }

def barExpr(using Quotes): Expr[Bar] =
  import quotes.reflect.*
  report.error(s"my error")
  report.error(s"my second error")
  '{ new Bar }
