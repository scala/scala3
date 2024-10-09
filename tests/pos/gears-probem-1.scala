import language.experimental.captureChecking
import caps.use

trait Future[+T]:
  def await: T

trait Channel[+T]:
  def read(): Ok[T]

class Collector[T](val futures: Seq[Future[T]^]):
  val results: Channel[Future[T]^{futures*}] = ???
end Collector

class Result[+T, +E]:
  def get: T = ???

case class Err[+E](e: E) extends Result[Nothing, E]
case class Ok[+T](x: T) extends Result[T, Nothing]

extension [T](fs: Seq[Future[T]^ @use])
  def awaitAll =
    val collector//: Collector[T]{val futures: Seq[Future[T]^{fs*}]}
       = Collector(fs)
    // val ch = collector.results // also errors
    val fut: Future[T]^{fs*} = collector.results.read().get // found ...^{caps.cap}