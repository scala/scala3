object Test extends dotty.runtime.LegacyApp {
  println(Stream.from(1).take(5).mkString)
}
