object Test extends dotty.runtime.LegacyApp {
  42 match {
    case Extractor(x) => println(x)
  }
}
