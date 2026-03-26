// scalajs: --skip
// (this is a JVM-only test)

trait A
class VC(val self: A) extends AnyVal
class Z {
  def vcARRAY(x: Array[VC]): Unit = {}
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[Z].getDeclaredMethods.foreach(m =>
      println(m)
      println(m.getGenericParameterTypes().mkString(","))
    )
