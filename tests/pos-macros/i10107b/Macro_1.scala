import scala.quoted._

inline def isTrue: Boolean = ${ isTrueImpl }
def isTrueImpl(using qctx: QuoteContext) = {
  Expr(true)
}

inline def oneOf(): String = {
  inline if isTrue then
    "foo"
  else
    "bar"
}
