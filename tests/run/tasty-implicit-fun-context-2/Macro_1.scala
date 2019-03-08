import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = given Reflection => Expr[X]
  type Tastier[X] = given Reflection => X

  implicit inline def foo: String =
    ${fooImpl}

  def fooImpl(implicit reflect: Reflection): given Reflection => Tastier[given Reflection => Macro[String]] = {
    '{"abc"}
  }

}
