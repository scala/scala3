import scala.quoted.*

def fooImpl(using Quotes): Expr[Any] =
  '{
    new AnyRef {
      type T = Unit
      def make: T = ()
      def take(t: T): Unit = ()
    }: {
      type T
      def make: T
      def take(t: T): Unit
    }
  }
