object Test extends dotty.runtime.LegacyApp {
  import Interpolation._
  2 match {
    case t"$x" => println(x)
  }
}
