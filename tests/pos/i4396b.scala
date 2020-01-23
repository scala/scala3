import scala.quoted._
def test with QuoteContext = {
  '{ case class Foo() }
}