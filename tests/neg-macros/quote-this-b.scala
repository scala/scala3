import scala.quoted._

class Foo {
  inline def k(): Unit = ${ Foo.impl[Any](this) } // error
  inline def l(that: Foo): Unit = ${ Foo.impl[Any](that) } // error
}

object Foo {
  def impl[T](x: Any): Expr[Unit] = '{}
}
