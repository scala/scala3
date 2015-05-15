import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  println(implicitly[TypeTag[Int]])
  println(implicitly[TypeTag[List[Int]]])
}
