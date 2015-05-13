import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class A { def foo = ??? }

object Test extends dotty.runtime.LegacyApp {
  val c = cm.classSymbol(classOf[A])
  println(c)
  println(c.fullName)
  println(c.info)
}
