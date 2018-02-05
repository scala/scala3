import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class A
trait B

object Test extends dotty.runtime.LegacyApp {
  val mutant = new A with B
  val c = cm.classSymbol(mutant.getClass)
  println(c)
  println(c.fullName)
  println(c.info)
}
