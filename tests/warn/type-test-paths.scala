

import scala.reflect.TypeTest

object Test {
  def main(args: Array[String]): Unit = {
    val p1: T = T1
    val p2: T = T1

    (p1.y: p1.X) match {
      case x: p2.Y => // warn: unchecked
      case x: p1.Y =>
      case _ =>
    }
  }

}

trait T {
  type X
  type Y <: X
  def x: X
  def y: Y
  given TypeTest[X, Y] = typeTestOfY
  protected def typeTestOfY: TypeTest[X, Y]
}

object T1 extends T {
  type X = Boolean
  type Y = true
  def x: X = false
  def y: Y = true
  protected def typeTestOfY: TypeTest[X, Y] = new {
    def unapply(x: X): Option[x.type & Y] = x match
      case x: (true & x.type) => Some(x)
      case _ => None
  }

}
