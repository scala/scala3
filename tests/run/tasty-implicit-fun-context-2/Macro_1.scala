import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = Reflection |=> Expr[X]
  type Tastier[X] = Reflection |=> X

  implicit inline def foo: String =
    ~fooImpl

  def fooImpl(implicit reflect: Reflection): Reflection |=> Tastier[Reflection |=> Macro[String]] = {
    '("abc")
  }
}
