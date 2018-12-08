import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = implicit Reflection => Expr[X]
  type Tastier[X] = implicit Reflection => X

  implicit inline def foo: String =
    ~fooImpl

  def fooImpl(implicit reflect: Reflection): implicit Reflection => Tastier[implicit Reflection => Macro[String]] = {
    '("abc")
  }

}
