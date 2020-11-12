import scala.quoted._
def test(using QuoteContext) = {
  val q = Type.of[String]
  Type.of[String]
}
