import language.experimental.captureChecking

trait Test:
  type T[-C^]
  type U[+C^]
  type C^
  type D^

  def foo[C^](x: T[C]): Unit = ???
  def bar(x: T[{}]): Unit = ???
  def baz(x: T[{caps.any}]): Unit = ???
  def foo2[C^](x: U[C]): Unit = ???
  def bar2(x: U[{}]): Unit = ???
  def baz2(x: U[{caps.any}]): Unit = ???

  def test =
    val t: T[{C}] = ???
    val t2: T[C] = ???
    val u: U[{D}] = ???
    val u2: U[D] = ???
    foo(t)
    bar(t)
    baz(t) // error
    foo2(u)
    bar2(u) // error
    baz2(u)
    foo(t2)
    bar(t2)
    baz(t2) // error
    foo2(u2)
    bar2(u2) // error
    baz2(u2)