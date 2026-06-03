// scalajs: --skip
// (JVM-only, generic signatures)

trait NotWorking:
  // return type args reversed compared to test case (b)
  def example[K, A, T[X,Y] <: Iterable[(X, Y)]](ab: String): T[A,K] = ???

object Test:
  def main(args: Array[String]): Unit =
    classOf[NotWorking].getMethods.filter(_.getName == "example").map(_.getGenericReturnType).foreach(println)
