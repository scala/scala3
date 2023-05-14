// scalajs: --skip

class A {
  val (a, b) = (1, 2)
}
object Test {
  def printFields(cls: Class[_]) =
    println(cls.getDeclaredFields.map(_.toString).sorted.toList.mkString("\n"))
  def printMethods(cls: Class[_]) =
    println(cls.getDeclaredMethods.map(_.toString).sorted.toList.mkString("\n"))

  def main(args: Array[String]): Unit = {
    println("# Fields of A:")
    printFields(classOf[A])
    println("# Methods of A:")
    printMethods(classOf[A])
  }
}
