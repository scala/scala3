class Test[T] {

  def f(x: Int) = try {
    ???
  }
  catch {
    case ex: scala.runtime.NonLocalReturnControl[T @scala.unchecked] =>
      ???
  }
}
