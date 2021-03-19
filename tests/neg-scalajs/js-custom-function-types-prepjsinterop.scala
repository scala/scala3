import scala.scalajs.js
import scala.scalajs.js.annotation.*

class BadFunctionIsClass extends js.Function {
  def apply(x: Int): Int // error
}
trait BadFunctionExtendsNonFunction extends js.Object {
  def apply(x: Int): Int // error
}
class SubclassOfFunction extends js.Function
trait BadFunctionExtendsSubclassOfFunction extends SubclassOfFunction {
  def apply(x: Int): Int // error
}
trait BadFunctionParametricMethod extends js.Function {
  def apply[A](x: A): A // error
}
trait BadFunctionOverloaded extends js.Function {
  def apply(x: Int): Int // error
  def apply(x: String): String // error
}
trait BadFunctionMultipleAbstract extends js.Function {
  def apply(x: Int): Int // error
  def foo(x: Int): Int
}
