//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Test {
  import scala.ref.WeakReference
  def unapply[T <: AnyRef](wr: WeakReference[T]): T? = {
    val x: T = wr.underlying.get
    if x != null then x else null
  }
}
