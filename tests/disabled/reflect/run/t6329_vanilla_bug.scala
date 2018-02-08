import scala.reflect.runtime.universe._
import scala.reflect.runtime._

object Test extends dotty.runtime.LegacyApp {
  println(classManifest[List[_]])
  println(scala.reflect.classTag[List[_]])
}
