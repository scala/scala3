import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  println(showRaw(typeOf[String]))
}
