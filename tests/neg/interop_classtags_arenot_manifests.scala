import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def classTagIsnotManifest[T: ClassTag] = {
    println(manifest[T]) // error
  }

  classTagIsnotManifest[Int]
  classTagIsnotManifest[String]
  classTagIsnotManifest[Array[Int]]
}
