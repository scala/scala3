import scala.quoted.{_, given}
def test(given QuoteContext) = {
  '{ case class Foo() }
}