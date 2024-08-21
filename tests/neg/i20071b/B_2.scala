
def test: Unit =
  foo[Scope] // ok
  bar[Scope] // error

  import Scope.i
  bar[Scope] // ok

