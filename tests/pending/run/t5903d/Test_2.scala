object Test extends dotty.runtime.LegacyApp {
  import Interpolation._
  42 match {
    case t"$x" => println(x)
  }
}
