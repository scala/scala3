object Test:
  def foo(f: (=> (Int, Int)) => Int) = ???
  foo((a, b) => a + b) // error // error
