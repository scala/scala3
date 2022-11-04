import annotation.{capability, constructorOnly}

@capability class Cap

class C(x: Cap, @constructorOnly y: Cap)

def test(a: Cap, b: Cap) =
  val f = () => C(a, b)
  val f_ok: {a, b} () -> {a} C = f
  val f_no1: {a, b} () -> C = f // error
  val f_no2: {a} () -> {a} C = f // error
  val f_no3: {b} () -> {a} C = f // error

  class D:
    val xz =
      println(a)
      1
    def yz =
      println(b)
      2
  val d = () => new D()
  val d_ok1: {a, b} () -> {a, b} D = d
  val d_ok2: () -> {a, b} D = d  // because of function shorthand
  val d_ok3: {a, b} () -> {b} D = d // error, but should work
