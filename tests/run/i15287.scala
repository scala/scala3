extension (x: Char)
  def f(someParam: String): String = "a"
  def f(min: Boolean, max: Int = 4): String = "b"

@main def Test() =
  f('c')(false)
  'c'.f(true)
  'c'.f("a")

