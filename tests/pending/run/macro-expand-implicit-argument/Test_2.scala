object Test extends dotty.runtime.LegacyApp {
  import Macros._
  println(array(1, 2, 3).toList)
}