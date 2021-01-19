import scala.quoted._
def test(using Quotes) = {
  '{ case class Foo() } // error
}