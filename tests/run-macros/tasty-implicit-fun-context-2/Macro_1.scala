import scala.quoted._
import scala.tasty.Reflection

object Foo {

  type Macro[X] = given StagingContext => Expr[X]
  type Tastier[X] = given StagingContext => X

  implicit inline def foo: String =
    ${fooImpl}

  def fooImpl(implicit staging: StagingContext): given StagingContext => Tastier[given StagingContext => Macro[String]] = {
    '("abc")
  }

}
