import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  def foo[T: TypeTag] = println(implicitly[TypeTag[T]])
  foo
}
