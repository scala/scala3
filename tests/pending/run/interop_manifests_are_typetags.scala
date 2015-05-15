import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  def manifestIsTypeTag[T: Manifest] = {
    println(typeOf[T])
  }

  manifestIsTypeTag[Int]
  manifestIsTypeTag[String]
  manifestIsTypeTag[Array[Int]]
}
