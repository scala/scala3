object Test2 {

  implicit def __1: implicit Int => String = s"implicit: ${implicitly[Int]}"
  implicit def __2: Int = 42

  def f: implicit String => Int = implicitly[String].length

  f: Int
}