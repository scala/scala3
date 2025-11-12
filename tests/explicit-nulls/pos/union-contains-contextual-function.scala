class Test:
  val x: ((a: String) ?=> Int) | Null = null

  val y: ((a: String) ?=> Int) | String = "hello"

  def f(g: ((a: String) ?=> Int) | Null = null) = ???
  