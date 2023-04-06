object Recursion:
  def foo(): Int =
    def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)
    fact(5)

  val n  = foo()