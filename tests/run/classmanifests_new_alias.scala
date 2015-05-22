
@deprecated("Suppress warnings", since="2.11")
object Test extends dotty.runtime.LegacyApp {
  type CM[T] = ClassManifest[T]
  println(implicitly[CM[Int]])
  println(implicitly[CM[Int]] eq Manifest.Int)
}
