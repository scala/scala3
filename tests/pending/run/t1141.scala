

import scala.language.reflectiveCalls

object Test extends dotty.runtime.LegacyApp {
  val foo = new {
    def apply(args : String*) = args foreach println
  }

  foo("var", "args")
}
