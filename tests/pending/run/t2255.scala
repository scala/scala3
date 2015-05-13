object Test extends dotty.runtime.LegacyApp {
  println(Stream.continually(Stream(1, 2, 3)).flatten.take(6).toList)
}
