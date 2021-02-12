import scala.quoted.*

inline def isTrue: Boolean = ${ isTrueImpl }
def isTrueImpl(using Quotes) = {
  Expr(true)
}

inline def oneOf(): String = {
  inline if isTrue then
    "foo"
  else
    "bar"
}
