import language.experimental.relaxedLambdaSyntax

def foo(x: Any) = ???

def test(xs: List[Int]) =
  xs.map: x => x
  foo:
    xs.map: x => x + 1

