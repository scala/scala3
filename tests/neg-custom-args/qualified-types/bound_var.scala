// This test checks that the bound predicate var in a qualified type is not
// erroneously confused a var bound inside a lambda.

def f(x: Int => Int): Boolean = ???

@main def main =
  summon[{x: Int with x == 2 && f(x => x)} <:< {x: Int with f(x => 2)}] // error
