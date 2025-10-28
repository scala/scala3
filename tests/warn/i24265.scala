//> using options -Wall -Werror

object test {
  inline def f(testFun: => Any) = testFun

  f {
    val i = 1
    summon[i.type <:< Int]
  }
}
