// scalajs: --skip
// (JVM-only, generic signatures)

trait Working:
  type M[X,Y] = Iterable[(X, Y)]
  def example[K, A, T[X,Y] <: M[X,Y]](ab: String): T[K,A] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[Working].getMethods.filter(_.getName == "example").map(_.getGenericReturnType).foreach(println)
