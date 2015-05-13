object Test extends dotty.runtime.LegacyApp {
  println(Array(Some(1), None, Some(2)).flatten.toList)
}
