import language.experimental.captureChecking
import caps.{use}

trait Future[+T]:
  def await: T

trait Channel[+T]:
  def read(): Ok[T]

class Collector[T, cap C](val futures: Seq[Future[T]^{C}]):
  val results: Channel[Future[T]^{C}] = ???
end Collector

class Result[+T, +E]:
  def get: T = ???

case class Err[+E](e: E) extends Result[Nothing, E]
case class Ok[+T](x: T) extends Result[T, Nothing]

extension [T, cap C](@use fs: Seq[Future[T]^{C}])
  def awaitAllPoly =
    val collector = Collector(fs)
    val fut: Future[T]^{C} = collector.results.read().get

extension [T](@use fs: Seq[Future[T]^])
  def awaitAll = fs.awaitAllPoly

def awaitExplicit[T](@use fs: Seq[Future[T]^]): Unit =
  awaitAllPoly[T, {fs*}](fs)
