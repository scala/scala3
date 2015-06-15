import scala.reflect.{ClassTag, classTag}

object Test extends dotty.runtime.LegacyApp {
  def mkArray[T: ClassTag] = Array[T]()
  def foo[T: ClassTag] = mkArray[T]
  println(foo[Int].getClass)
}
