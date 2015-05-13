

@deprecated("Suppress warnings", since="2.11")
object Test extends dotty.runtime.LegacyApp {
  class X[T: ClassManifest] {
    val a = Array.ofDim[T](3, 4)
  }
  val x = new X[String]
  x.a(1)(2) = "hello"
  assert(x.a(1)(2) == "hello")
}
