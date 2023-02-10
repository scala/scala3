import scala.quoted.*
class Foo {
  def f[T2: Type](e: Expr[T2])(using Quotes) = e match {
    case '{ type t <: Tuple; $x: *:[Int, t] } => // FIXME infer bounds of tail
      Type.of[ *:[Int, t] ]
  }
}
