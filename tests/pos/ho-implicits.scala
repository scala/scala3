object Test2 {

  implicit def __1: Int |=> String = s"implicit: ${implicitly[Int]}"
  implicit def __2: Int = 42

  def f: String |=> Int = implicitly[String].length

  f: Int
}