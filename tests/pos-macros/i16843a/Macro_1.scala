import scala.quoted.*

case class Foo(x: Int)

inline def foo = ${ fooImpl }

def fooImpl(using Quotes) =
  val tmp = '{
    1 match
      case x @ (y: Int) => 0
  }

  '{}
