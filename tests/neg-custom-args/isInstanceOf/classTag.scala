import scala.reflect.ClassTag

object IsInstanceOfClassTag {
  def safeCast[T: ClassTag](x: Any): Option[T] = {
    x match {
      case x: T => Some(x)
      case _ => None
    }
  }

  def foo(x: Any): Boolean =
    x.isInstanceOf[List[String]]  // error
}