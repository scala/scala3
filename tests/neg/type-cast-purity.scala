//> using options -language:experimental.erasedDefinitions

object O:
  opaque type T = String

  transparent inline def make: T =
    println("effect")
    "value"

// shouldn't emit E129: Pure Expression In Statement Position
object Statement:
  O.make

def consume(erased value: O.T): Unit = ()

def test(): Unit =
  consume(O.make) // error
