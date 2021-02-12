import scala.quoted._

inline def isTrue: Boolean = true
transparent inline def oneOf: Any = inline if isTrue then isTrue else "bar"
def test1 = oneOf
