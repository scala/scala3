// scalajs: --skip
// (JVM-only, generic signatures)

trait NotWorking:
  def example[K, A, T[X,Y] <: Iterable[(X, Y)]](ab: String): T[K,A] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[NotWorking].getMethods.filter(_.getName == "example").map(_.getGenericReturnType).foreach(println)
