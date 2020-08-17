package scala

package object implicits {
  @deprecated("scala.implicits.Not has been renamed scala.util.Not", "0.27.0")
  type Not[A] = scala.util.Not[A]
}
