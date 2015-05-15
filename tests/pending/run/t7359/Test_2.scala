import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  typeOf[Cyclic].members
  println("ok")
}