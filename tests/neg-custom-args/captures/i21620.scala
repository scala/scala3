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

def test2(x: C^) =
  def foo() =
    x
    ()
  val f = () =>
    // println() //  uncomenting would give an error, but with
                 //  a different way of handling curried functions should be OK
    () => foo()
  val _: () ->{} () ->{x} Unit = f   // error, but could be OK
  ()
