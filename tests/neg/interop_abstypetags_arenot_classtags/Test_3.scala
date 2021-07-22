import scala.reflect.runtime.universe._
import scala.reflect.classTag

object Test extends App {
  def weakTypeTagIsnotClassTag[T: WeakTypeTag] = {
    println(classTag[T]) // error
  }

  // weakTypeTagIsnotClassTag[Int]
  // weakTypeTagIsnotClassTag[String]
  // weakTypeTagIsnotClassTag[Array[Int]]
}
