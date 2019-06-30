import scala.quoted._
def test given QuoteContext = {
  '{ case class Foo() }
}