object Test {
  def call(k: (Int, Int) => Unit): Unit = ???
  def test = call({ case (x, y) => ()})
}
