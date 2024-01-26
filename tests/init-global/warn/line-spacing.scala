object A {
  def a: Int =
    B
      .s.length
}

object B {
  val s: String = s"${A.a}a"
}

