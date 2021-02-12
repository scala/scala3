import scala.quoted.*
class Foo {
  def test(using Quotes) = '{
    ${3} // error
    ${new Object} // error
    ${"abc"} // error
    ${()} // error
    ${new Foo} // error
  }

  def unary_$ : Int = 9
}
