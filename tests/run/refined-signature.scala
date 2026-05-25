// scalajs: --skip
// (this is a JVM-only test)

trait B
trait Cov[+T]

class Z {
  def cov_125(x: Cov[B]): Unit = {}
  def cov_126(x: Cov[B] { type X }): Unit = {}
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[Z].getDeclaredMethods.map(_.getGenericParameterTypes().mkString(",")).foreach(println)
