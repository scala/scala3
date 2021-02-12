import scala.quoted.*
def test(using Quotes) = {
  '{ case class Foo() } // error
}