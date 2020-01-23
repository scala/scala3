object Test {
  trait T; trait Q
  val a: T ?=> Q ?=> Int = 1

  given Q = new Q {}
  val i1: Int = a.with(new T{})
  given T = new T {}
  val i2: Int =  a
  val i3: Int =  a2

  def a2 with (t: T) with (q: Q) : Int = 1

}
