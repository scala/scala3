object Test2 {

  implicit def __1: given Int => String = s"implicit: ${implicitly[Int]}"
  implicit def __2: Int = 42

  def f: given String => Int = implicitly[String].length

  f: Int
}