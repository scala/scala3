object Foo {
  def test(s: String) = {
    (s: Seq[Char]) match {
        case Seq('f', 'o', 'o', ' ', rest1*) =>
          rest1
        case Seq('b', 'a', 'r', ' ', ' ', rest2*) =>
          rest2
        case _ =>
          s
    }
  }
}
