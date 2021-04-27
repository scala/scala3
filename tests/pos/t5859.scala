
class A {
  def f(xs: List[Int], ys: AnyRef*) = ()
  def f(xs: AnyRef*) = ()

  f()
  f(List[AnyRef]()*)
  f(List()*)
  f(Nil*)
  // f(Array()*)  // undetermined ClassTag
  f(Array[AnyRef]()*)
  f(List(1))
  f(List(1), Nil*)
  // f(List(1), Array()*)  // undetermined ClassTag
}
