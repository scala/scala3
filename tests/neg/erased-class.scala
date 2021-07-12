import language.experimental.erasedDefinitions
erased class AA
erased class BB extends AA // ok

@main def Test =
  val f1: Array[AA] = ??? // error
  def f2(x: Int): Array[AA] = ??? // error
  def bar: AA = ???  // ok
  val baz: AA = ??? // ok
