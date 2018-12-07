object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyVal](wr: WeakReference[T]): Option[T] = { // error
    val x = wr.underlying.get
    if (x != null) Some(x) else None // The failure used to be here, since x had type T <: AnyVal. but now `x: T|Null`, so
    // `x != null` is valid.
  }
}
