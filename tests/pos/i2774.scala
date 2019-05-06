object Test {
  trait T; trait Q
  val a: given T => given Q => Int = 1

  implied for Q = new Q {}
  val i1: Int = a given (new T{})
  implied for T = new T {}
  val i2: Int =  a

  def a given (t: T) given (q: Q): Int = 1

}
