object ConfusingErrorMessage {
  def m1() = ()
  def m2()() = ()
  def m3()()() = ()
  def f3()(i: Int)() = ()
  def i3()(using d: DummyImplicit)() = ()
  m1 // error
  m1()
  m2 // error
  m2() // error
  m3 // error
  m3() // error
  m3()() // error
  m3()()()
  f3 // error
  f3() // error
  f3()(2) // error
  f3()(2)()
  i3 // error
  i3() // error
  i3()()
}
