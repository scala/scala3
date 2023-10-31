trait A {
  val actualType: Class[?]
}
trait B extends A {
  final val actualType = classOf[Boolean]
}

object Test extends B {
  def main(args: Array[String]): Unit = {
    println(actualType)
  }
}
