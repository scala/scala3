import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait BadThisFunction1 extends js.ThisFunction {
  def apply(): Int
}
trait BadThisFunction2 extends js.ThisFunction {
  def apply(args: Int*): Int
}
class A {
  val badThisFunction1: BadThisFunction1 = () => 42 // error
  val badThisFunction2: BadThisFunction2 = args => args.size // error
}
