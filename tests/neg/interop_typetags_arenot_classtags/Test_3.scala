import scala.reflect.runtime.universe._
import scala.reflect.classTag

object Test extends App {
  def typeTagIsnotClassTag[T: TypeTag] = {
    println(classTag[T]) // error
  }

  // typeTagIsnotClassTag[Int]
  // typeTagIsnotClassTag[String]
  // typeTagIsnotClassTag[Array[Int]]
}
