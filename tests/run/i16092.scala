
class A(a: Int)

class B extends A(1):
  val a = 2 // ok

@main def Test =
  assert(B().a == 2)
