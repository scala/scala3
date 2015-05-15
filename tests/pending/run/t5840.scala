import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  reify {
    class C[T <: String with Singleton]
  }
}
