class C
def test(x: C^) =
  val f = () =>
    def foo() =
      x
      ()
    println(s"hey: $x")
    () => foo()
  val _: () -> () ->{x} Unit = f // error
  ()
