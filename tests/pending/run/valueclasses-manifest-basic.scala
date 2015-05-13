class Foo(val x: Int) extends AnyVal

object Test extends dotty.runtime.LegacyApp {
  println(manifest[Foo])
}
