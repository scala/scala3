object Test extends dotty.runtime.LegacyApp {
  println(new C().blackbox)
  println(new C().refinedBlackbox)
  println(new C().whitebox)
  println(new C().refinedWhitebox)
}