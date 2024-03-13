trait A:
  private type M

def foo(a: A{type M = Int}) =
  val _: a.M = ??? // error was crash
