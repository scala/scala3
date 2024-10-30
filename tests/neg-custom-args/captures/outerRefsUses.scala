class IO
def test(io: IO^) =
  class C:
    def foo() = () =>
      val x: IO^{this} = io
      ()
  val c = new C
  val _: C^{io} = c // ok
  val _: C = c // error
  ()
