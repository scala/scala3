object Test {

  def a(lis: Set[Int] | Set[String]) = {}
  a(Set(1))
  a(Set(""))

  def b(lis: List[Set[Int] | Set[String]]) = {}
  b(List(Set(1)))
  b(List(Set("")))

  def c(x: Set[Any] | Array[Any]) = {}
  c(Set(1))
  c(Array(1))
}
