object C:
  def f(a: => Int): Int =
    lazy val a: Int = 10 + b
    lazy val b: Int = 20 + a
    b

  val n = f(10)
