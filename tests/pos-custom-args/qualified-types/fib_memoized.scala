case class DependentMap[K, V](p: (K, V) => Boolean):
  def put(n: K, v: V with p(n, v)): Unit = ???
  def get(n: K): Option[{res: V with p(n, res)}] = ???

def fib(n: Int): {res: Int with
  res == (
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)
  )
} =
  if n == 0 then 0
  else if n == 1 then 1
  else fib(n - 1) + fib(n - 2)

val cache: DependentMap[Int, Int] with (cache == DependentMap[Int, Int]((k, v) => v == fib(k))) =
  DependentMap[Int, Int]((k, v) => v == fib(k))

def fibMemoized(n: Int): {res: Int with res == fib(n)} =
  cache.get(n) match
    case Some(res) =>
      res
    case None =>
      val res =
        if n == 0 then 0
        else if n == 1 then 1
        else fibMemoized(n - 1) + fibMemoized(n - 2)
      cache.put(n, res.runtimeChecked)
      res.runtimeChecked

@main def main = ()
