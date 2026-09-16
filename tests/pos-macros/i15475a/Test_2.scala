package x
import scala.language.unsafeNulls
def hello = {
  xtransform {
    val a: Seq[Generic[?]] = null
    a
      .foreach { to =>
        to.mthd()
      }
  }
}

trait Generic[+T] {
  def mthd(): Generic[T] = this
}
