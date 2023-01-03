// scalajs: --skip

class Foo extends Ann {
  override def bar = 3
  override def baz = 4
  def annotationType = classOf[Ann]
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Foo
    val y: Ann = x
    val z: Int @Ann(1) = 1
    val zz: Int @Ann() = 1
    val ann: java.lang.annotation.Annotation = new Ann {
      override def bar = 3
      override def baz = 4
      def annotationType = classOf[Ann]
    }
  }
}
