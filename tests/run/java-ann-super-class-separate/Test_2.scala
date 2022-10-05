// scalajs: --skip

class Foo extends Ann_1 {
  override def bar = 3
  override def baz = 4
  def annotationType = classOf[Ann_1]
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Foo
    val y: Ann_1 = x
    val z: Int @Ann_1(1) = 1
    val zz: Int @Ann_1() = 1
    val ann: java.lang.annotation.Annotation = new Ann_1 {
      def bar = 3
      def baz = 4
      def annotationType = classOf[Ann_1]
    }
  }
}
