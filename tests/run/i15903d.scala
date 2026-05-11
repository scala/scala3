// scalajs: --skip
// (JVM-only, generic signatures)

trait NotWorking:
  def example[A, X[T] <: Iterable[X[List[T]]]](s: String): X[A] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[NotWorking].getMethods.filter(_.getName == "example").map(_.getGenericReturnType).foreach(println)
