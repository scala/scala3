final class DependentMap[K, V](val p: (K, V) => Boolean):
  def put(n: K, v: V with p(n, v)): Unit = ???
  def get(n: K): Option[{res: V with p(n, res)}] = ???

def fib(n: Int): {res: Int with
  if n == 0 then res == 0
  else if n == 1 then res == 1
  else res == fib(n - 1) + fib(n - 2)
} =
  ??? // TODO: add a mem_fib implementation once we flow-typing

@main def main =
  val map = DependentMap[Int, Int]((k, v) => v == fib(k))
  val v = map.get(10).get
  ???
