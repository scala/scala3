import scala.quoted._
def test(v: String)(using Quotes): Any =
  Type.of : Type[v.type] // error
  Type.of[v.type] // error
