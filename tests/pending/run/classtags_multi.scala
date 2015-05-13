import scala.reflect.{ClassTag, classTag}

object Test extends dotty.runtime.LegacyApp {
  println(classTag[Int])
  println(classTag[Array[Int]])
  println(classTag[Array[Array[Int]]])
  println(classTag[Array[Array[Array[Int]]]])
  println(classTag[Array[Array[Array[Array[Int]]]]])
}
