//> using options -Werror -deprecation -feature

def foo: Unit =
  object O:
    sealed abstract class A
  class B extends O.A
  class C extends O.A

  val x: O.A = ???
  x match
  case _: B => ???
  case _: C => ???
