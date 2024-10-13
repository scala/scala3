class C
def test(x: C^, y: C^) =
  class D {
    println(x)
    def foo() = println(y)
  }
  val d = D()
  val _: D^{y} = d // error
  val _: D = d // error

  val f = () => println(D())
  val _: () ->{x, y} Unit = f // ok
  val _: () -> Unit = f // error

  def g = () =>
    println(x)
    () => println(y)
  val _: () ->{x, y} () ->{y} Unit = g
  val _: () -> () -> Unit = g // error

