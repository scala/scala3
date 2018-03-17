import scala.reflect.ClassTag

object Test {
  def foo[T <: AnyRef : ClassTag] = new Array[T](42)
  def main(args: Array[String]): Unit = {
    val x: Array[String] = foo[String]
  }
}