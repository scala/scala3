object Test {
  type Dummy[A] = A

  def a(d: Dummy[String]) = ()
  def a(d: Dummy[Int]) = ()

  implicit def dummy[A]: Dummy[A] = null.asInstanceOf[A]
  def m(x: List[String])(implicit d: Dummy[String]) = "string"
  def m(x: List[Int])(implicit d: Dummy[Int]) = "int"

  m(List(1, 2, 3))
  m(List("a"))
}
