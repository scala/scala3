//> using options -Yexplicit-nulls
import language.experimental.magic
object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyRef](wr: WeakReference[T]): T? = {
    val x: T = wr.underlying.get
    if (x != null) x else null
  }
}
