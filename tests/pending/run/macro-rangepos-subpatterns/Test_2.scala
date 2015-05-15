object Test extends dotty.runtime.LegacyApp {
  42 match {
    case Extractor(a) => println(a)
  }
}
