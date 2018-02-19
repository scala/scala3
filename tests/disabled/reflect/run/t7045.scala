import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C
class D { self: C => }

object Test extends dotty.runtime.LegacyApp {
  val d = cm.staticClass("D")
  println(d.selfType)
  d.info
  println(d.selfType)
}
