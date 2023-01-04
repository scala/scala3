import scala.quoted.*

object Foo:
  def baz(using Quotes): Unit = '{
    def f[T](x: T): T = ${ identity('{ x: T }) }
  }
