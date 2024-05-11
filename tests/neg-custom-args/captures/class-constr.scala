import annotation.{capability, constructorOnly}

class Cap extends caps.Capability

class C(x: Cap, @constructorOnly y: Cap)

def test(a: Cap, b: Cap) =
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
  val d_ok1: () ->{a, b} D^{a, b} = d
  val d_ok2: () -> D^{a, b} = d  // because of function shorthand
  val d_ok3: () ->{a, b} D^{b} = d // error, but should work
