object Test {
  unused val foo1: Nothing = ??? // error
  unused def foo2: Nothing = ??? // error
  unused def foo3(): Nothing = ??? // error
  def foo4(unused a: Nothing): Unit = () // error
}