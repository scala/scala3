object Test {
  trait Bar

  dependent def test(t: Any) =
    t match { case _ => 1 }

  // def f2(x: Bar): "z" =
  def f2(x: Bar): 1 =
    test(1)
}
