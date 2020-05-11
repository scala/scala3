object Crash {
  def f(a: String, b: String, c: Int = 0): Int ?=> String = ""
  given Int = ???
  f(b = "b", a = "a")
}