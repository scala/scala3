//> using options -language:experimental.erasedDefinitions

object O:
  opaque type T = String

  transparent inline def make: T =
    println("effect")
    "value"

def consume(erased value: O.T): Unit = ()

def test(): Unit =
  consume(O.make) // error
