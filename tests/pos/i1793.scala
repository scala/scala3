object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyRef](wr: WeakReference[T]): Option[T] = {
    val x: T = wr.underlying.get
    if (x != null) Some(x) else None
  }
}
