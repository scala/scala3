final class DependentMap[K, V](val p: (K, V) => Boolean):
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

@main def main =
  val map = DependentMap[Int, Int]((k, v) => v == fib(k))
  val v = map.get(10).get
  ???
