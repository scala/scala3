object Thunk {
  private[this] val impl =
    ((x: Any) => x).asInstanceOf[(=> Any) => Function0[Any]]

  def asFunction0[A](thunk: => A): Function0[A] = impl(thunk).asInstanceOf[Function0[A]]
}

@main def Test =
  var i = 0
  val f1 = { () => i += 1; "" }
  assert(Thunk.asFunction0(f1()) eq f1)
  val f2 = { () => i += 1; i }
  assert(Thunk.asFunction0(f2()) eq f2)
  val f3 = { () => i += 1 }
  assert(Thunk.asFunction0(f3()) eq f3)
