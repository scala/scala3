//> using options -Werror

import scala.reflect.ClassTag

object IsInstanceOfClassTag {
  def safeCast[T: ClassTag](x: Any): Option[T] = {
    x match {
      case x: T => Some(x) // TODO error: deprecation waring
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    safeCast[List[String]](List[Int](1)) match {
      case None =>
      case Some(xs) =>
        xs.head.substring(0)
    }

    safeCast[List[?]](List[Int](1)) match {
      case None =>
      case Some(xs) =>
        xs.head.substring(0) // error
    }
  }
}
