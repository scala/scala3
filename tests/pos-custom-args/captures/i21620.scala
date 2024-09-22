class C
def test(x: C^) =
  def foo() =
    x
    ()
  val f = () =>
    // println() //  uncomenting would give an error, but with
                 //  a different way of handling curried functions should be OK
    () => foo()
  val _: () -> () ->{x} Unit = f
  ()
