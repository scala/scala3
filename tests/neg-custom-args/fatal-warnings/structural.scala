import scala.reflect.Selectable.reflectiveSelectable

package p1 {

object test123 {
  type A = { def a: Int }
  def f(a: A): A = a
}

object structural2 {
  type A = { def a: Int }

  type B = {
    def b: Int
  }

  type AB = A & B

  def f(ab: AB): AB = ab

  f(new {
    def a = 43
    def b = 42
  })
}
}

package p2 {
object RClose {
  type ReflectCloseable = { def close(): Unit }
  def withReflectCloseable[T <: ReflectCloseable, R](s: T)(action: T => R): R =
    try {
      action(s)
    } finally {
      s.close()
    }
}
}

package p3 {

object Test {
  def idMap[C[_],T](m: { def map[U](f: T => U): C[U] }): C[T] = m.map(t => t) // error: polymorphic refinement method map without matching type in parent Object is no longer allowed // error: Structural access not allowed

  def main(args: Array[String]): Unit = {
    idMap(Some(5)) // error: type mismatch: found Some[Int], required Object{map: [U](f: Any => U): Any}
    idMap(Responder.constant(5)) // error: type mismatch: found Responder[Int], required Object{map: [U](f: Any => U): Any}
  }
}
}
package p4 {

trait A { self: Any { def p: Any } =>
  def f(b: => Unit): Unit = {}
  f { p } // OK
}
}

package p5 {
// t2810
object Test {
  val closeable1: { def close(): Unit } = new scala.io.Source { val iter: Iterator[Char] = "".iterator }
  val closeable2: { def close(): Unit } = new java.io.Closeable { def close() = {} }
}
}

package p6 {

  class Refinements {
    val y: { val x: T; type T }  // error: deprecated warning: illegal forward reference in refinement; now illegal
  }

}
