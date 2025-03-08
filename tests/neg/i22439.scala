
@main def test() = println:
  given Int = 42
  def f(implicit i: Int, j: Int) = i + j
  def g(using i: Int, j: Int) = i + j
  val x: Int = f
  f() // error f() missing arg
  g() // error g(given_Int, given_Int)() doesn't take more params
  f // ok implicits
  g // ok implicits
  f(j = 27) // error missing argument for parameter i of method f
  f(using j = 27) // ok, explicit supplemented by implicit
  g(using j = 27) // ok, explicit supplemented by implicit

  def h(using i: Int, s: String) = s * i
  h // error
  h(using i = 17) // error

  val vs = List((42, 27))
  val (xs, ys) = vs.unzip
  val (ws, zs) = vs.unzip() // error!
