object Test extends dotty.runtime.LegacyApp {
  println(Module.value)
  Module.value = "world"
  println(Module.value)
}