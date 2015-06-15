object Test extends dotty.runtime.LegacyApp {
  import NewQuasiquotes._
  SomeTree match {
    case nq"$x + $y" => println((x, y))
  }
}
