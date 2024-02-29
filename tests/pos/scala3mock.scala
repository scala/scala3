class MockFunction1[T1]:
  def expects(v1: T1 | Foo): Any = ???
  def expects(matcher: String): Any = ???

def when[T1](f: T1 => Any): MockFunction1[T1] = ???

class Foo

def main =
    val f: Foo = new Foo
    when((x: Foo) => "").expects(f)
