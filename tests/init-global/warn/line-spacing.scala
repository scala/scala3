object A {
  def a: Int =
    B
      .s.length // warn
}

object B {
  val s: String = s"${A.a}a"
}
