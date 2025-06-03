trait Async extends caps.Capability

class Future[+T](x: () => T)(using val a: Async)

class Collector[T](val futs: Seq[Future[T]^]):
  def add(fut: Future[T]^{futs*}) = ???

def main() =
  given async: Async = ???
  val futs = (1 to 20).map(x => Future(() => x))
  val col = Collector(futs)
  col.add(Future(() => 25)) // error
  val col1: Collector[Int] { val futs: Seq[Future[Int]^{async}] }
    = Collector(futs)
  col1.add(Future(() => 25)) // error


