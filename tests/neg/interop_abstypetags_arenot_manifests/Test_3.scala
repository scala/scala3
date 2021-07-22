import scala.reflect.runtime.universe._

object Test extends App {
  def weakTypeTagIsnotManifest[T: WeakTypeTag] = {
    println(manifest[T]) // error
  }

  // weakTypeTagIsnotManifest[Int]
  // weakTypeTagIsnotManifest[String]
  // weakTypeTagIsnotManifest[Array[Int]]
}
