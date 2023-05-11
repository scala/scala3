import scala.quoted.*

class Foo {

  def f(using Quotes): Unit = '{
    def bar[T](x: T): T = x
    bar[this.type] {
      this  // error
    }
  }

}
