object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyRef](wr: WeakReference[T]): Option[T] = {
    val x: T|Null = wr.underlying.get
    val x2 = x.asInstanceOf[T]
    if (x2 != null) Some(x2) else None
  }
}
