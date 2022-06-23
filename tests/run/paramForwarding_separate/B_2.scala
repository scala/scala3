// scalajs: --skip

class B(member: Int) extends SubA(member) {
  def getMember = member
}

object Test {
  def printFields(cls: Class[_]) =
    println(cls.getDeclaredFields.map(_.toString).sorted.toList.mkString("\n"))

  def main(args: Array[String]): Unit = {
    val a = new A(10)
    val subA = new SubA(11)
    val b = new B(12)

    println("# Fields in A:")
    printFields(classOf[A])
    println("# Fields in SubA:")
    printFields(classOf[SubA])
    println("# Fields in B:")
    printFields(classOf[B])
  }
}
