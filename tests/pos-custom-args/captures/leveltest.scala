class Cap extends caps.ExclusiveCapability

class A

def T =
  def test(x: A^): A^{x} = ???

  def foo(): A^ = ???

  val a: A^ = ???

  val x = test(foo())
  val _: A^ = x

  val y = test(a) :: Nil
  val _: List[A^{a}] = y

  def z =
    val b = test(a)
    Nil.::(b)
  val _: List[A^{a}] = z



