import language.experimental.captureChecking

trait Future[+T]:
  def await: T

trait Channel[T]:
  def read(): Either[Nothing, T]

class Collector[T, c^](val futures: Seq[Future[T]^{c}]):
  val results: Channel[Future[T]^{c}] = ???
end Collector

extension [T, C^](fs: Seq[Future[T]^{C}])
  def awaitAll =
    val collector: Collector[T, C]{val futures: Seq[Future[T]^{C}]}
       = Collector(fs)
    // val ch = collector.results // also errors
    val fut: Future[T]^{C} = collector.results.read().right.get // error

    val ch = collector.results
    val item = ch.read()
    val r = item.right
    val fut2: Future[T]^{C} = r.get // error