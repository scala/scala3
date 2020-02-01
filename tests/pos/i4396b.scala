import scala.quoted._
def test(using QuoteContext) = {
  '{ case class Foo() }
}