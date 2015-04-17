object Test {
  def test2: Int = {
    var ds: String = null
    def s = {
      ds = "abs"
      ds
    }
    s.length
  }
}
