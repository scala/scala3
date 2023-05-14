import scala.quoted._
trait Foo
object Foo {
  def apply_impl(using Quotes): Expr[Any] = '{
    new Foo {
      private def foo: String = ???
      def bar: Any = ${ '{ foo }; ??? }
    }
  }
}
