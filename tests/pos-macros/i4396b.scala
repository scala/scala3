import scala.quoted._
def test(using Scope) = {
  '{ case class Foo() }
}