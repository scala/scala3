import scala.quoted._

class Foo {

  def f(using QuoteContext): Unit = '{
    def bar[T](x: T): T = x
    bar[
      this.type  // error
      ] {
      this  // error
    }
  }

}
