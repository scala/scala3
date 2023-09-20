//> using options -experimental

@print("foo")
def foo(): Unit = ()

@print("foo") @print("foo")
def fooFoo(): Unit = ()

@print("foo") @print("bar")
def fooBar(): Unit = ()

@print("bar") @print("foo")
def barFoo(): Unit = ()

@main def Test =
  foo()
  println()
  fooFoo()
  println()
  fooBar()
  println()
  barFoo()
