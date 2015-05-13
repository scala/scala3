object Test extends dotty.runtime.LegacyApp {
  println(scala.reflect.runtime.universe.reify(new Object().getClass))
}
