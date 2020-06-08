package classes
import scala.language.experimental.macros
class C1(val x1: Int) extends AnyVal

class C2(val x2: Int) extends AnyVal
object C2

case class C3(x: Int)

case class C4(x: Int)
object C4

object M {
  implicit class C5(x: Int)
}

case class C6(private val x: Int)

class C7(x: Int)

class C8(private[this] val x: Int)

class C9(private[this] var x: Int)

class C10(s: => String)

class C11 {
  def foo: Int = macro ???
  inline def foo: Int = ???
}

class C12 {

  class Context: // Dummy scala.reflect.macros.Context
    type Expr[T]

  def foo1(x: Int): Int = macro foo1Impl
  def foo1(x: Int): Int = ???

  def foo2(x: Int, y: String): Int = macro foo2Impl
  def foo2(x: Int, y: String): Int = ???

  def foo1Impl(context: Context)(x: context.Expr[Int]): context.Expr[Int] = ???
  def foo2Impl(context: Context)(x: context.Expr[Int], y: context.Expr[String]): context.Expr[Int] = ???

}

object N {
  val anonClass = new C7(42) {
    val local = ???
  }
  val anonFun = List(1).map { i =>
    val local = 2
    local + 2
  }
}
