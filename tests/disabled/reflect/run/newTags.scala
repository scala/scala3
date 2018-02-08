import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}

object Test extends dotty.runtime.LegacyApp {
  println(ru.typeOf[List[Int]])
  def foo[T: ru.TypeTag] = {
    println(ru.typeOf[T])
    println(implicitly[ApiUniverse#TypeTag[T]])
  }
  foo[Map[String, String]]
}
