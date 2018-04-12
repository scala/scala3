object Main {
  def f(m: Map[String, Boolean]) = {}
  println(f(Map('a' -> true)))  // error
  println(this.f(Map('a' -> true)))  // error
}
