//> using options -experimental -Yno-experimental

class Bar:
  @memoize
  def fib(n: Int): Int =
    println(s"compute fib of $n")
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)
  //> val fibCache$macro$1: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
  //> @memoize def fib(n: Int): Int =
  //>   if fibCache$macro$1.contains(n) then fibCache$macro$1(n)
  //>   else
  //>     val res: Int =
  //>       println(s"compute fib of $n")
  //>       if n <= 1 then n
  //>       else fib(n - 1) + fib(n - 2)
  //>     fibCache$macro$1.update(n, res)
  //>     res

@main def Test =
  val t = new Bar
  assert(t.fib(3) == 2)
  assert(t.fib(4) == 3)
