object O {
  def m(x: Any*) = ()

  def n2(l: List[Int] | List[String]): Unit = m(l)
  def n1(l: List[Int] | List[String]): Unit = m(l*)
  def m2(l: List[Int] & List[String]): Unit = m(l)
  def m1(l: List[Int] & List[String]): Unit = m(l*)
}