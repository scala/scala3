import scala.quoted._
class Foo:
  def foo[T: Type](using Quotes): Unit = '{ // level 1
    given Quotes = ???

    Type.of[T]

    '{ ??? : T } // level 2

    '{ // level 2
      given Quotes = ???
      Type.of[T]
      '{ ??? : T } // level 3
    }
  }
