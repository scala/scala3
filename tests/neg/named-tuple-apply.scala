class Foo:
  def apply: () => Int = () => 2

def test =
  (apply = () => 1)(())  // error // error
  (apply = () => 1, foo = 2).foo  // error
  (apply = () => 1, foo = 2).apply(1)  // error

  val foo = Foo()
  foo() // error (error message could be better)
