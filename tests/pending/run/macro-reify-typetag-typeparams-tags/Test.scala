import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  def fooTypeTag[T: TypeTag] = {
    println(implicitly[TypeTag[T]])
    println(implicitly[TypeTag[List[T]]])
  }
  fooTypeTag[Int]
}
