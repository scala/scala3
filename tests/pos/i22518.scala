sealed trait Foo[T]
class Bar extends Foo[?]

def mkFoo[T]: Foo[T] =
  ???

def test: Unit =
  mkFoo match
    case _ => ()
