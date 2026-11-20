import scala.quoted.*

inline def matchNothingLambda(inline f: Int => String): Unit = ${ matchNothingLambdaImpl('f) }

def matchNothingLambdaImpl(f: Expr[Int => String])(using Quotes): Expr[Unit] =
  f match
    case '{ (x: Int) => ??? } => '{}
    case _ => quotes.reflect.report.errorAndAbort("No match")
