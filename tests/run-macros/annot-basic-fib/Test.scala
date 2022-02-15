import scala.collection.mutable.Map

class Bar:
  @memoize
  def fib(n: Int): Int =
    println(s"compute fib of $n")
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

@main def Test =
  val t = new Bar
  assert(t.fib(3) == 2)
  assert(t.fib(4) == 3)
