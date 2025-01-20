import language.experimental.captureChecking

trait Future[+T]:
  def await: T

trait Channel[T]:
  def read(): Either[Nothing, T]

class Collector[T](val futures: Seq[Future[T]^]):
  val results: Channel[Future[T]^{futures*}] = ???
end Collector

extension [T](fs: Seq[Future[T]^])
  def awaitAll =
    val collector: Collector[T]{val futures: Seq[Future[T]^{fs*}]}
       = Collector(fs)
    // val ch = collector.results // also errors
    val fut: Future[T]^{fs*} = collector.results.read().right.get // found ...^{caps.cap}