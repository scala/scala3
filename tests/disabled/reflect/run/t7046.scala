import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

sealed class C
class D extends C
class E extends C

object Test extends dotty.runtime.LegacyApp {
  val c = cm.staticClass("C")
  println(c.knownDirectSubclasses)
  c.info
  println(c.knownDirectSubclasses)
}
