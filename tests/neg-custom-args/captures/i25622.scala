trait IO:
  def println(): Unit = ()
def test(io: IO^): Unit =
  trait A0:
    def callIO(): Unit
  class A extends A0:
    val x: A = this
    def callIO(): Unit = io.println() // error

  val a: A^ = A()
  val b: A0^{} = a.x
  val apparentlyNotPure: () ->{} Unit = () => b.callIO()  // boom