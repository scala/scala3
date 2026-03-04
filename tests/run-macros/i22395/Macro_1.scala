import scala.quoted._

inline def test(): String = ${testImpl}
def testImpl(using Quotes) = {
  import quotes.reflect._
  val fooSymbol = TypeRepr.of[Foo[Int]].typeSymbol
  val nestedSymbol = fooSymbol.typeMember("Nested")

  Expr(TypeRepr.of[Foo[Int]].memberType(nestedSymbol).toString)
}


trait Foo[X]:
  sealed abstract class Nested extends Foo[Int]
