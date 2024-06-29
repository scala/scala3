
//> using options -Wunused:all

import scala.util.Try
import scala.concurrent.* // warn
import scala.collection.Set
class C {
  def ss[A](using Set[A]) = println() // warn
  private def f = Try(42).get
  private def z: Int =  // warn
    Try(27 + z).get
  def g = f + f
  def k =
    val i = g + g
    val j = i + 2 // warn
    i + 1
  def c = C()
  import scala.util.Try // warn
}
class D {
  def d = C().g
}
