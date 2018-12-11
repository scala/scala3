trait Fun[L[_]]

object O1 {
  trait N[X]
}

object O2 {
  def bar: Fun[O1.N] = ???
}
