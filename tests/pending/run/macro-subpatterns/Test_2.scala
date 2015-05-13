object Test extends dotty.runtime.LegacyApp {
  42 match {
    case Extractor(a @ Extractor(b @ Extractor(c))) => println(a); println(b); println(c)
  }
}
