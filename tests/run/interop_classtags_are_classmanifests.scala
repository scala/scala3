import scala.reflect.ClassTag

@deprecated("Suppress warnings", since="2.11")
object Test extends dotty.runtime.LegacyApp {
  def classTagIsClassManifest[T: ClassTag] = {
    println(classManifest[T])
  }

  classTagIsClassManifest[Int]
  classTagIsClassManifest[String]
  classTagIsClassManifest[Array[Int]]
}
