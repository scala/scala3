case class X(s: String)

object Test extends dotty.runtime.LegacyApp {
  import scala.reflect.runtime.universe._
  println(typeOf[X])
}
