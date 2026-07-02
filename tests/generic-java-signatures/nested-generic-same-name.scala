trait Outer[A] {
  case class Same[A]()
  case class Diff[B]()

  val same: Same[A] = Same()
  val diff: Diff[A] = Diff()
}

object Test:
  def main(arguments: Array[String]): Unit =
    classOf[Outer[?]].getDeclaredMethods.sortBy(_.getName())
      .filter(_.getName.contains("_setter_"))
      .foreach(setter => {
        println(setter.getName)
        println(setter.getGenericParameterTypes().mkString(","))
        println(setter.toGenericString)
      })
