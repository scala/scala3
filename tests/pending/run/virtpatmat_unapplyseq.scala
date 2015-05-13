object Test extends dotty.runtime.LegacyApp {
  List(1,2,3) match {
    case Seq(x, y, z) => println(x * y * z)
  }
}
