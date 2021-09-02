class Foo
class Bar extends Foo

inline def thingy(a: Int = 0, b: Int = 0, inline c: Foo = new Bar) = {
  inline c match {
    case _: Bar =>
  }
}

def x = 1

def test = thingy(b = x)
