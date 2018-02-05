import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  println(implicitly[TypeTag[Int]])
  println(implicitly[TypeTag[Array[Int]]])
  println(implicitly[TypeTag[Array[Array[Int]]]])
  println(implicitly[TypeTag[Array[Array[Array[Int]]]]])
  println(implicitly[TypeTag[Array[Array[Array[Array[Int]]]]]])
}
