class Test:
  val x: ((a: String) ?=> Int) | Null = null

  val x2: ((a: String) ?=> Int) | Null = (s: String) ?=> 42

  val y: ((a: String) ?=> Int) | String = "hello"

  def f(g: ((a: String) ?=> Int) | Null = null) = ???
  