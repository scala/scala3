import scala.quoted._
import scala.tasty.Tasty

object Foo {

  type Macro[X] = implicit Tasty => Expr[X]
  type Tastier[X] = implicit Tasty => X

  implicit inline def foo: String =
    ~fooImpl

  def fooImpl(implicit tasty: Tasty): implicit Tasty => Tastier[implicit Tasty => Macro[String]] = {
    '("abc")
  }

}
