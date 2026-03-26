// Extra example 2 from OOPSLA 26
case class DepMap[K, V](p: (K, V) => Boolean):
  def put(n: K, v: V with p(n, v)): Unit = ???
  def get(n: K): Option[{res: V with p(n, res)}] = ???

def fib(n: Int): {r: Int with r == (if n <= 1 then 1 else fib(n - 1) + fib(n - 2))} =
  if n <= 1 then 1 else fib(n - 1) + fib(n - 2)

val cache: (DepMap[Int, Int] with (cache == DepMap[Int, Int]((k, v) => v == fib(k)))) =
  DepMap[Int, Int]((k, v) => v == fib(k))

def fibMemo(n: Int): {res: Int with res == fib(n)} =
  cache.get(n) match
    case Some(res) => res
    case None =>
      val res: (Int with (res == fib(n))) =
        if n <= 1 then 1 else fibMemo(n - 1) + fibMemo(n - 2)
      cache.put(n, res)
      res
