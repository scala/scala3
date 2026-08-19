//> using options -Yexplicit-nulls
import language.experimental.magic
trait T[U] {
  def x: T[_ <: U]
}

object T {
  def unapply[U](t: T[U]): T[_ <: U]? = t.x
}

object Test {
  def f[W](t: T[W]) = t match {
    case T(T(_)) => ()
  }
}

