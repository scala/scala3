//> using options -Werror

object A1 {
  def f1(implicit x1: String, x2: Int = 2): Unit = ()
}

object A2 {
  given String = "s"
  def f2 = A1.f1
}