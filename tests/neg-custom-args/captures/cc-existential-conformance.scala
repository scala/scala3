class A
class B

type Fun[T] = A -> T

def test() =
  val x: A -> (x: A) -> B^ = ???
  val y: A -> Fun[B^] = x // error
  val z: A -> A -> B^ = y // ok

def test2() =
  val x: (x: A) -> B^ = ???
  val y: Fun[B^] = x // error
  val z: A -> B^ = y // ok
