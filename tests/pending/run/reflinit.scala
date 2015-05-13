import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  val tt2 = typeOf[List[Int]]
  println(tt2)
}
