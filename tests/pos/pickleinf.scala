class Bar[N] {
  def bar(name: N, dummy: Int = 42): N = name
}

object Test {
  def test(): Unit = {
    (new Bar).bar(10)
  }
}
