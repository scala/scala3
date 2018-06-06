package scala.reflect

trait AbstractTypeTag[T] {
  def unapply(arg: Any): Option[T]
}

object AbstractTypeTag {
  implicit def classTag[T](implicit ct: ClassTag[T]): AbstractTypeTag[T] = new AbstractTypeTag[T] {
    def unapply(arg: Any): Option[T] = ct.unapply(arg) // TODO try to remove by inlining, otherwise special case in compiler
  }
}
