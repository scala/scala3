import scala.quoted.*
object A {
  inline def foo(a: Any): Unit = ${ crashOnInline }
  def crashOnInline(using Quotes): Expr[Unit] = ???
}
