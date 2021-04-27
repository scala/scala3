package outer
import nested.*

object indent2 {
  val x2: Int = indent1.inner.x
  val y2: Int = indent1.y
}
