import scala.annotation.experimental

@experimental class A
@experimental type X
@experimental type Y = Int
@experimental opaque type Z = Int

def test1(
  p1: A, // error: class A is marked @experimental and therefore ...
  p2: List[A], // error: class A is marked @experimental and therefore ...
  p3: X, // error: type X is marked @experimental and therefore ...
  p4: Y, // error: type Y is marked @experimental and therefore ...
  p5: Z, // error: type Z is marked @experimental and therefore ...
): A = ??? // error: class A is marked @experimental and therefore ...

@experimental def test2(
  p1: A,
  p2: List[A],
  p3: X,
  p4: Y,
  p5: Z,
): A = ???

class Test1(
  p1: A, // error
  p2: List[A], // error
  p3: X, // error
  p4: Y, // error
  p5: Z, // error
) {}

@experimental class Test2(
  p1: A,
  p2: List[A],
  p3: X,
  p4: Y,
  p5: Z,
) {}

trait Test3(
  p1: A, // error
  p2: List[A], // error
  p3: X, // error
  p4: Y, // error
  p5: Z, // error
) {}

@experimental trait Test4(
  p1: A,
  p2: List[A],
  p3: X,
  p4: Y,
  p5: Z,
) {}
