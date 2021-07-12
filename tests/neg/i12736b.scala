object Test {
  def apply[S](r: Any)(using DoesntExist): Any = r // error

  def test(o: Option[Any]) =
    o.map(x => Test(doesntExist, x)) // error
}
