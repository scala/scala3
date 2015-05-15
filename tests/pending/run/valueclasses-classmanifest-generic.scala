class Foo[T](val x: T) extends AnyVal

@deprecated("Suppress warnings", since="2.11")
object Test extends dotty.runtime.LegacyApp {
  println(classManifest[Foo[String]])
}
