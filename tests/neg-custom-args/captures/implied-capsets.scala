import caps.*

class C1 extends Stateful
class C2 extends Stateful:
  var x: Int = 0
class C3 extends Stateful, ExclusiveCapability
class C4 extends Stateful, SharedCapability

def Test: Unit =
  val c1 = C1()
  val _: C1^{} = c1
  val c2 = C2()
  val _: C2^ = c2
  val _: C2^{} = c2 // error
  val c3 = C3()
  val _: C3^ = c3
  val _: C3^{} = c3 // error
  val c4 = C4()
  val _: C4^ = c4
  val _: C4^{} = c4 // error
  def foo(x: C1, y: C2, z: C3, u: C4) =
    val _: C1^ = x
    val _: C2^ = y
    val _: C3^ = z // error
    val _: C4^ = u

  foo(c1,
      c2, // error
      c3,
      c4)

