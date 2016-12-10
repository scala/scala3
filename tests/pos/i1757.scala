case class B[T](b: List[Int]) {
  var s: B[Int] = ???
  def cpy[X](b: List[Int] = b): B[X] = new B[X](b)
  s.cpy()
  s.copy()
}
