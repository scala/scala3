import scala.quoted._

object Foo:
  inline def isTrue: Boolean = true
inline def oneOf: String = inline if Foo.isTrue then "foo" else "bar"
def test1 = oneOf
