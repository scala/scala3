import scala.quoted._

inline def isTrue: Boolean = true
inline def oneOf: String = inline if isTrue then "foo" else "bar"
def test1 = oneOf
