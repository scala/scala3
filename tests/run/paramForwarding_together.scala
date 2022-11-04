// scalajs: --skip

class A(val member: Int) {
  def getAMember = member
}

class SubA(member: Int) extends A(member) {
  def getSubAMember = member
}

class B(member: Int) extends SubA(member) {
  def getBMember = member
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
