object O{
  def m(x: Any*) = ()
  def n(l: List[Int] | List[String]): Unit = m(l*)
  def n2(l: List[Int] & List[String]): Unit = m(l*)
}