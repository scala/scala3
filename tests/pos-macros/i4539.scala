import scala.quoted._
def test(using QuoteContext) = {
  val q = Type[String]
  Type[String]
}
