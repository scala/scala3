object Test extends dotty.runtime.LegacyApp {
  println(new C().blackbox)
  println(new C().whitebox)
}