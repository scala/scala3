@deprecated("Suppress warnings", since="2.11")
object Test extends dotty.runtime.LegacyApp {
  println(classManifest[Int])
  println(classManifest[Int] eq Manifest.Int)
}
