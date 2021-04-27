import scala.quoted.*
class Foo {
  def f[T2](t: Type[T2])(using Quotes) = t match {
    case '[ *:[Int, t2] ] =>
      Type.of[ *:[Int, t2] ]
  }
}
