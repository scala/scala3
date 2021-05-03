import scala.annotation.experimental

@experimental // error
class A

@experimental // error
class B extends A

@experimental // error
type X

@experimental // error
type Y = Int

@experimental // error
opaque type Z = Int

def test(
  p1: A, // error
  p2: List[A], // error
  p3: X, // error
  p4: Y, // error
  p5: Z, // error
): Unit =
  new A // error
  new B // error
  val i1 = identity[X] // error // error
  val i2 = identity[A] // error // error
  ()
