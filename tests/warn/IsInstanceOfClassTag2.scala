

import scala.reflect.TypeTest

object IsInstanceOfClassTag {
  def safeCast[T](x: Any)(using TypeTest[Any, T]): Option[T] = {
    x match {
      case x: T => Some(x)
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    safeCast[List[String]](List[Int](1)) match { // warn
      case None =>
      case Some(xs) =>
    }

    safeCast[List[?]](List[Int](1)) match {
      case None =>
      case Some(xs) =>
    }
  }
}

