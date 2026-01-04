import annotation.{capability, constructorOnly}

class Cap extends caps.SharedCapability

class C(x: Cap, @constructorOnly y: Cap)

val a: Cap = Cap()
val b: Cap = Cap()

def test() =
  val f = () => C(a, b)
  val f_ok: () ->{a, b} C^{a} = f
  val f_no1: () ->{a, b} C = f // error
  val f_no2: () ->{a} C^{a} = f // error
  val f_no3: () ->{a} C^{a} = f // error

  class D:
    val xz =
      println(a)
      1
    def yz =
      println(b)
      2
  val d = () => new D()
  val d_ok1: () ->{a, b} D^{a, b} = d // ok
  val d_ok2: () ->{a} D^{b} = d  // ok
  val d_ok3: () -> D^{a, b} = d // error
