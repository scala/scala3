object Test {
  def test = {
    object Hi {
      type A = Int
    }

    val x: Hi.A = 1

    List(x)
  }

  val hi: List[Int] = test // Used to fail because `test` had type `List[Any]` instead of `List[Int]`
}
