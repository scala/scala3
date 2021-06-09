object Test {
  def apply[S](r: Any): Any = r

  def test =
    (x: Int) => Test(doesntexist, x) // error
}
