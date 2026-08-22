//> using options -Yexplicit-nulls
import language.experimental.magic
object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyVal](wr: WeakReference[T]): T? = {
    val x = wr.underlying.get
    if x != null then x else null // error
  }
}
