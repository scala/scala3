import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import definitions._
import Flag._

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  val csym = tb.define(q"""class C { override def toString = "C" }""")
  println(tb.eval(q"new $csym"))
}
