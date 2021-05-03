import scala.annotation.experimental

@experimental // FIXME ERROR
class A

@experimental // FIXME ERROR
class B extends A

@experimental // FIXME ERROR
type X

@experimental // FIXME ERROR
type Y = Int

@experimental // FIXME ERROR
opaque type Z = Int

def test(
  p1: A, // error
  p2: List[A], // FIXME ERROR
  p3: X, // error
  p4: Y, // error
  p5: Z, // error
): Unit =
  new A // error
  new B // error
  val i1 = identity[X] // error // error
  val i2 = identity[A] // error // error
  ()
