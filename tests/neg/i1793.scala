object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyVal](wr: WeakReference[T]): Option[T] = {
    val x = wr.underlying.get
    if (x != null) Some(x) else None // error
  }
}
