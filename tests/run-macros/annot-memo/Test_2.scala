//> using options -experimental

class Bar:
  @memoize
  def fib(n: Int): Int =
    println(s"compute fib of $n")
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)
  //> private val fibCache$macro$1: mutable.Map[Int, Int] = mutable.Map.empty[Int, Int]
  //> @memoize def fib(n: Int): Int =
  //>  fibCache$macro$1.getOrElseUpdate(n, {
  //>     println(s"compute fib of $n")
  //>     if n <= 1 then n
  //>     else fib(n - 1) + fib(n - 2)
  //>   })

  //> private val fibCache$macro$1: mutable.Map[(n : Int), Int] = mutable.Map.empty[(n : Int), Int] // FIXME: leaks (n : Int)
  //> @memoize def fib(n: Int): Int =
  //>  fibCache$macro$1.getOrElseUpdate(n, {
  //>     println(s"compute fib of $n")
  //>     if n <= 1 then n
  //>     else fib(n - 1) + fib(n - 2)
  //>   })

  @memoize
  def fib(n: Long): Long =
    println(s"compute fib of $n")
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

  @memoize
  def fib(n: Short): Long =
    println(s"compute fib of $n")
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

@main def Test =
  val t = new Bar
  assert(t.fib(3) == 2)
  assert(t.fib(4) == 3)
  assert(t.fib(3L) == 2L)
  assert(t.fib(4L) == 3L)
  assert(t.fib(3: Short) == 2L)
  assert(t.fib(4: Short) == 3L)
