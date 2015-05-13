import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  def fooNoTypeTag[T] = {
    println(implicitly[WeakTypeTag[T]])
    println(implicitly[WeakTypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}
