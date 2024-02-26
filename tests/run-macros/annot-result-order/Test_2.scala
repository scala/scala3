import scala.language.experimental.mode

@print("foo")
def foo(): Unit = ()

@print("foo") @print("bar")
def fooBar(): Unit = ()

@print("bar") @print("foo")
def barFoo(): Unit = ()

@main def Test =
  foo()
  fooBar()
  barFoo()
