import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  def fooTypeTag[T: TypeTag] = {
    println(implicitly[WeakTypeTag[T]])
    println(implicitly[WeakTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}
