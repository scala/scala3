import caps.fresh

class A
class B

type Fun[T] = A -> T

def test() =
  val x: A -> (x: A) -> B^{fresh} = ???
  val y: A -> Fun[B^] = x // error
  val z: A -> (x: A) -> B^ = y // error

def test2() =
  val x: (x: A) -> B^{fresh} = ???
  val y: Fun[B^] = x // error
  val z: (x: A) -> B^ = y // error
