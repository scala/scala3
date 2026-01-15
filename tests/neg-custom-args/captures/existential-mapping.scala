import language.experimental.captureChecking

class A
class C
type Fun[X] = (x: C^) -> X
type IFun[X] = (x: C^) => X
def Test =
  val x1: (x: C^) -> C^ = ???
  val _:  (x: C^) -> C = x1 // error

  val x2: C^ -> C^ = ???
  val _:  C^ -> C = x2 // error

  val x3: A^ -> (x: C^) -> C^ = ???
  val _: A^ -> (x: C^) -> C = x3 // error

  val x4: A^ -> C^ -> C^ = ???
  val _: A^ -> C^ -> C = x4 // error

  val x5: A^ -> Fun[C^] = ???
  val _: A^ -> (x: C^) -> C = x5 // error

  val x6: A^ -> IFun[C^] = ???
  val _: A^ -> (x: C^) => C = x6 // error

  val y1: (x: C^) => C^ = ???
  val _:  (x: C^) => C = y1 // error

  val y2: C^ => C^ = ???
  val _:  C^ => C = y2 // error

  val y3: A^ => (x: C^) => C^ = ???
  val _: A^ => (x: C^) => C = y3 // error

  val y4: A^ => C^ => C^ = ???
  val _: A^ => C^ => C = y4 // error

  val y5: A^ => Fun[C^] = ???
  val _: A^ => (x: C^) -> C = y5 // error

  val y6: A^ => IFun[C^] = ???
  val _: A^ => (x: C^) => C = y6 // error

  val z1: A^ => Array[C^] = ??? // ok

  val z2: (x: A^) => Array[C^] = ??? // ok

