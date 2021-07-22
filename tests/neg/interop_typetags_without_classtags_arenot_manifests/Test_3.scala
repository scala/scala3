import scala.reflect.runtime.universe._

object Test extends App {
  def typeTagWithoutClassTagIsnotManifest[T: TypeTag] = {
    println(manifest[T]) // error
  }

  // typeTagWithoutClassTagIsnotManifest[Int]
  // typeTagWithoutClassTagIsnotManifest[String]
  // typeTagWithoutClassTagIsnotManifest[Array[Int]]
}
