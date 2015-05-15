import Macros._

object Test extends dotty.runtime.LegacyApp {
  println(Macros.foo.x)
}