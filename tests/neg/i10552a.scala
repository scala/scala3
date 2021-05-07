transparent inline def foo[T]: Int = 10

def test =
  foo[List] // error
