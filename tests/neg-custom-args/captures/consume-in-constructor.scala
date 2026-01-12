import caps.{any, Stateful}

class B

class A1(val b: B^):
  val bb: B^ = B()

class A2(consume val b: B^):
  val bb: B^ = B()

class A3(consume val b: B^) // was error now ok
class A4(consume val b: B^) extends Stateful { var x: Int = 1 } // ok
def Test =
  val b: B^ = B()
  val a1 = A1(b)
  val _: A1^{any, b} = a1
  println(b)  // OK   since a1's type mentions `b` explicitly
  val a2 = A2(b)
  val _: A2^{any, b} = a2
  println(b) // error
  println(a1) // error, since `b` was consumed before
  println(a2) // OK since b belongs to a2

def Test2 =
  val b: B^ = B()
  val a1: A1^ = A1(b)
  println(b) // error, b is hidden in the type of a1





