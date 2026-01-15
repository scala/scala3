class IO
val io: IO^ = IO()
def test() =
  class C:
    def foo() = () =>
      val x: IO^{this} = io
      ()
  val c = new C
  val _: C^{io} = c // ok
  val _: C = c // error
  ()
