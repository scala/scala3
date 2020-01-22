object O{
  def m(x: Any*) = ()
  def n(l: List[Int] | List[String]): Unit = m(l: _*)
  def n2(l: List[Int] & List[String]): Unit = m(l: _*)
}