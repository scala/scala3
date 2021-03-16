def go(x: Int): Unit =
  go  // error
  go  // error
  go  // error

def foo: Unit =
  (x: Int) => go(x) // warning

