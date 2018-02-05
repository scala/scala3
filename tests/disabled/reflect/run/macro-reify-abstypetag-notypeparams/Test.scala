import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  println(implicitly[WeakTypeTag[Int]])
  println(implicitly[WeakTypeTag[List[Int]]])
}
