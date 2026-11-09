trait Async extends caps.SharedCapability

class Future[+T](x: () => T)(using val a: Async)

class Collector[T, c^](val futs: Seq[Future[T]^{c}]):
  def add(fut: Future[T]^{c}) = ???

def main() =
  given async: Async = ???
  val futs = (1 to 20).map(x => Future(() => x))
  val col = Collector(futs)
  col.add(Future(() => 25))
  val col1: Collector[Int, {async}] { val futs: Seq[Future[Int]^{async}] }
    = Collector(futs)
  col1.add(Future(() => 25))


